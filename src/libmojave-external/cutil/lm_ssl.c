/*
 * Interface to OpenSSL.  This looks just like a standard socket
 * API, except that a few of the functions require certificates.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2003 Mojave Group, Caltech
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation,
 * version 2.1 of the License.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * Additional permission is given to link this library with the
 * OpenSSL project's "OpenSSL" library, and with the OCaml runtime,
 * and you may distribute the linked executables.  See the file
 * LICENSE.libmojave for more details.
 *
 * Author: Jason Hickey
 * @email{jyh@cs.caltech.edu}
 * @end[license]
 */
#include <stdio.h>
#include <memory.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>

#ifdef WIN32
#  include <winsock.h>
#  define SHUT_RD 0
#  define SHUT_WR 1

typedef long socklen_t;
#else /* !WIN32 */
#  include <unistd.h>
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <fcntl.h>

typedef int SOCKET;

#define closesocket close
#endif /* !WIN32 */

#define Nothing ((value) 0)

extern void uerror (char * cmdname, value arg) Noreturn;

#define uerr(cmd) uerror(cmd, Nothing)

void enter_blocking_section(void);
void leave_blocking_section(void);

/*
 * Inet addresses are strings.
 */
static value alloc_inet_addr(uint32 addr)
{
    value a;
    a = alloc_string(sizeof(uint32));
    *(uint32 *)a = addr;
    return a;
}

#ifdef SSL_ENABLED

/************************************************************************
 * SSL routines.
 */

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/evp.h>

/*
 * Struct for the socket.
 *   server: is this a server socket?
 *   fd: file descriptor for the socket
 *   context: the SSL context
 *   ssl:
 */
typedef enum {
    SSL_INFO_MASTER,
    SSL_INFO_SLAVE
} SslInfoKind;

typedef struct {
    SslInfoKind kind;
    SOCKET fd;
    SSL_CTX *context;
    SSL *ssl;
} SslInfo;

/************************************************
 * ML interface to addresses.
 */

/*
 * Print an error message.
 */
static void print_errors()
{
    char buffer[1024];
    unsigned long err;
    const char *s;

    while((err = ERR_get_error()))
        fprintf(stderr, "lm_ssl: %s\n", ERR_error_string(err, buffer));
}

/*
 * Create a new context.
 */
static SSL_CTX *lm_ssl_ctx_new(const char *keyfile)
{
    SSL_CTX *context;

    /* Create a new context and initialize */
    context = SSL_CTX_new(SSLv23_method());
    if(context == 0) {
        print_errors();
        failwith("lm_ssl_ctx_new: can't allocate context");
    }

    /* Load our certificate */
    if(SSL_CTX_use_certificate_chain_file(context, keyfile) == 0) {
        fprintf(stderr, "keyfile: %s\n", keyfile);
        print_errors();
        failwith("lm_ssl_ctx_new: can't assign certificate");
    }

    /* Password */
    if(SSL_CTX_use_PrivateKey_file(context, keyfile, SSL_FILETYPE_PEM) == 0) {
        fprintf(stderr, "keyfile: %s\n", keyfile);
        print_errors();
        failwith("lm_ssl_ctx_new: can't set private key");
    }

    return context;
}

/*
 * Add the DH file to the context.
 */
static void lm_ssl_ctx_dhfile(SSL_CTX *context, const char *dhfile)
{
    BIO *bio;
    DH *ret;

    /* Set the DH parameters */
    bio = BIO_new_file(dhfile, "r");
    if(bio == 0) {
        print_errors();
        failwith("lm_ssl_ctx_new: can't open DH file");
    }

    /* Read the file */
    ret = PEM_read_bio_DHparams(bio, NULL, NULL, NULL);
    BIO_free(bio);
    if(SSL_CTX_set_tmp_dh(context, ret) < 0) {
        print_errors();
        failwith("lm_ssl_ctx_new: can't set DH params");
    }
}

/*
 * Custom blocks.
 */
#define SslInfo_val(v)   ((SslInfo *) Data_custom_val(v))

static int ssl_info_compare(value v1, value v2)
{
    SslInfo *info1 = SslInfo_val(v1);
    SslInfo *info2 = SslInfo_val(v2);

    return info1->fd == info2->fd ? 0 : info1->fd < info2->fd ? -1 : 1;
}

static long ssl_info_hash(value v)
{
    return (long) SslInfo_val(v);
}

static void ssl_finalize(value v_info)
{
    SslInfo *info;
    int code;

    info = SslInfo_val(v_info);
    if(info->ssl) {
        SSL_free(info->ssl);
        info->ssl = 0;
    }
    if(info->kind == SSL_INFO_MASTER && info->context) {
        SSL_CTX_free(info->context);
        info->context = 0;
    }
    if(info->fd >= 0) {
        closesocket(info->fd);
        info->fd = -1;
    }
}

/*
 * Pass info in a custom block.
 */
static struct custom_operations win_handle_ops = {
    "ssl_socket",
    ssl_finalize,
    ssl_info_compare,
    ssl_info_hash,
    custom_serialize_default,
    custom_deserialize_default
};

static value ssl_info_new(SslInfoKind kind, SOCKET fd, SSL_CTX *context, SSL *ssl)
{
    value v = alloc_custom(&win_handle_ops, sizeof(SslInfo), 0, 1);
    SslInfo *info = SslInfo_val(v);
    info->kind = kind;
    info->fd = fd;
    info->context = context;
    info->ssl = ssl;
    return v;
}

/************************************************************************
 * Public functions.
 */

/*
 * Say whether SSL is enabled.
 */
value lm_ssl_enabled(value x)
{
    return Val_true;
}

/*
 * Start the SSL server.
 */
value lm_ssl_init(value x)
{
    SSL_load_error_strings();
    SSL_library_init();
    return Val_unit;
}

/*
 * Create a new socket.
 */
value lm_ssl_socket(value v_keyfile)
{
    SOCKET s;
    SSL_CTX *context;
    int one = 1;

    /* Add context */
    context = lm_ssl_ctx_new(String_val(v_keyfile));

    /* Open the socket */
    s = socket(PF_INET, SOCK_STREAM, 0);
    if(s < 0) {
        SSL_CTX_free(context);
        failwith("lm_ssl_socket: socket call failed");
    }

    /* Allow the address to be reused */
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (void *) &one, sizeof(one));

    return ssl_info_new(SSL_INFO_MASTER, s, context, 0);
}

/*
 * Reuse an existing socket.
 */
value lm_ssl_serve(value v_fd, value v_keyfile, value v_dhfile)
{
    SOCKET s;
    SSL_CTX *context;
    SslInfo *info;
    value v_info;
    int one = 1;

    /* Add context */
    context = lm_ssl_ctx_new(String_val(v_keyfile));

    /* Open the socket */
    s = Int_val(v_fd);

    /* Allow the address to be reused */
    setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (void *) &one, sizeof(one));

    /* Allocate the context */
    v_info = ssl_info_new(SSL_INFO_MASTER, s, context, 0);
    info = SslInfo_val(v_info);

    /* Set the Diffie-Helman file */
    lm_ssl_ctx_dhfile(info->context, String_val(v_dhfile));

    return v_info;
}

/*
 * Get an integer describing the socket.
 */
value lm_ssl_fd(value v_info)
{
    SslInfo *info = SslInfo_val(v_info);
    return Val_int(info->fd);
}

/*
 * Bind the socket to an address.
 */
value lm_ssl_bind(value v_info, value v_addr, value v_port)
{
    SslInfo *info;
    struct sockaddr_in sin;

    /* Get the address */
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = *(uint32 *)v_addr;
    sin.sin_port = htons((short) Int_val(v_port));

    /* Perform the bind */
    info = SslInfo_val(v_info);
    if(bind(info->fd, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
		  uerr("lm_ssl_bind");
    }

    return Val_unit;
}

/*
 * Get the address of the socket.
 */
value lm_ssl_get_addr(value v_info)
{
    SslInfo *info;
    struct sockaddr_in sin;
    socklen_t size;
    value a, addr;

    /* Get the address */
    info = SslInfo_val(v_info);
    size = sizeof(sin);
    if(getsockname(info->fd, (struct sockaddr *) &sin, &size) < 0 || sin.sin_family != AF_INET) {
        uerr("lm_ssl_get_addr: getsockname");
    }

    /* Allocate the address */
    a = alloc_inet_addr(sin.sin_addr.s_addr);
    Begin_root(a);
    addr = alloc_small(2, 0);
    Field(addr, 0) = a;
    Field(addr, 1) = Val_int(ntohs(sin.sin_port));
    End_roots();
    return addr;
}

/*
 * Listen.
 */
value lm_ssl_listen(value v_info, value v_dhfile, value v_count)
{
    SslInfo *info;

    info = SslInfo_val(v_info);
    lm_ssl_ctx_dhfile(info->context, String_val(v_dhfile));
    listen(info->fd, Int_val(v_count));
    return Val_unit;
}

/*
 * Accept a connection on the socket.
 */
value lm_ssl_accept(value v_info)
{
    SslInfo *info;
    int fd, code;
    SSL *ssl;

    /* Listen on the server socket */
    info = SslInfo_val(v_info);
    enter_blocking_section();
    fd = accept(info->fd, (struct sockaddr *) 0, 0);
    leave_blocking_section();
    if(fd < 0) {
        uerr("lm_ssl_accept");
    }

#ifndef WIN32
    /* Set the close-on-exec flag */
    fcntl(fd, F_SETFD, FD_CLOEXEC);
#endif

    /* Start SSL operations */
    ssl = SSL_new(info->context);
    if(ssl == 0) {
        closesocket(fd);
        print_errors();
        failwith("lm_ssl_accept");
    }

    /* Set the descriptor */
    code = SSL_set_fd(ssl, fd);
    if(code <= 0) {
        closesocket(fd);
        SSL_free(ssl);
        print_errors();
        failwith("lm_ssl_accept: set_fd failed");
    }

    /* Negotiate */
    code = SSL_accept(ssl);
    if(code <= 0) {
        closesocket(fd);
        SSL_free(ssl);
        print_errors();
        failwith("lm_ssl_accept: negotiation failed");
    }

    /* Allocate a new struct */
    return ssl_info_new(SSL_INFO_SLAVE, fd, info->context, ssl);
}

/*
 * Make a connection on the socket.
 */
value lm_ssl_connect(value v_info, value v_addr, value v_port)
{
    SslInfo *info;
    struct sockaddr_in sin;
    int fd, code;
    SSL *ssl;

    /* Check to make sure not connected */
    info = SslInfo_val(v_info);
    if(info->ssl)
        failwith("lm_ssl_connect: already connected");

    /* Get the address */
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = *(uint32 *)v_addr;
    sin.sin_port = htons((short) Int_val(v_port));

    /* Make the connection */
    enter_blocking_section();
    code = connect(info->fd, (struct sockaddr *) &sin, sizeof(sin));
    leave_blocking_section();
    if(code < 0) {
        uerr("lm_ssl_connect");
    }

    /* Start SSL operations */
    ssl = SSL_new(info->context);
    if(ssl == 0) {
        print_errors();
        failwith("lm_ssl_connect");
    }

    /* Set the descriptor */
    code = SSL_set_fd(ssl, info->fd);
    if(code <= 0) {
        SSL_free(ssl);
        print_errors();
        failwith("lm_ssl_connect: set_fd failed");
    }

    /* Negotiate */
    code = SSL_connect(ssl);
    if(code <= 0) {
        SSL_free(ssl);
        print_errors();
        failwith("lm_ssl_connect: negotiation failed");
    }

    info->ssl = ssl;
    return Val_unit;
}

/*
 * Read some data from the connection.
 */
value lm_ssl_read(value v_info, value v_string, value v_off, value v_len)
{
    int off, len, amount;
    SslInfo *info;
    char *buf;

    info = SslInfo_val(v_info);
    buf = String_val(v_string);
    off = Int_val(v_off);
    len = Int_val(v_len);
    enter_blocking_section();
    amount = SSL_read(info->ssl, buf + off, len);
    leave_blocking_section();
    return Val_int(amount);
}

/*
 * Write some data to the connection.
 */
value lm_ssl_write(value v_info, value v_string, value v_off, value v_len)
{
    int off, len, amount;
    SslInfo *info;
    char *buf;

    info = SslInfo_val(v_info);
    buf = String_val(v_string);
    off = Int_val(v_off);
    len = Int_val(v_len);
    enter_blocking_section();
    amount = SSL_write(info->ssl, buf + off, len);
    leave_blocking_section();
    return Val_int(amount);
}

/*
 * Flush the connection.
 */
value lm_ssl_flush(value v_info)
{
    SslInfo *info;
    BIO *bio;

    info = SslInfo_val(v_info);
    enter_blocking_section();
    bio = SSL_get_wbio(info->ssl);
    if(bio)
        /* XXX: BUG! 
         * According to the man page:
         * BIO_flush(), because it can write data may return 0 or -1 indicating that the call should be retried later
         * in a similar manner to BIO_write().  The BIO_should_retry() call should be used and appropriate action
         * taken is the call fails.
         */
        BIO_flush(bio);
    leave_blocking_section();
    return Val_unit;
}

/*
 * Shutdown the connection.
 */
value lm_ssl_shutdown(value v_info)
{
    SslInfo *info;
    int code;

    /* Shutdown the connection */
    info = SslInfo_val(v_info);
    code = SSL_shutdown(info->ssl);

    /*
     * If we called shutdown first, the return code is always 0.
     * In this case, shutdown the TCP connection
     * and try again.
     */
    if(code == 0) {
        shutdown(info->fd, SHUT_WR);
        code = SSL_shutdown(info->ssl);
    }

    /* SSL struct is no longer needed */
    SSL_free(info->ssl);
    info->ssl = 0;
    return Val_unit;
}

/*
 * Close the connection.
 */
value lm_ssl_close(value v_info)
{
    ssl_finalize(v_info);
    return Val_unit;
}

#else /* !SSL_ENABLED */

/*
 * Unencrypted socket connection.
 */
typedef struct {
    SOCKET fd;
} SslInfo;

/*
 * Custom blocks.
 */
#define SslInfo_val(v)   ((SslInfo *) Data_custom_val(v))

static int ssl_info_compare(value v1, value v2)
{
    SslInfo *info1 = SslInfo_val(v1);
    SslInfo *info2 = SslInfo_val(v2);

    return info1->fd == info2->fd ? 0 : info1->fd < info2->fd ? -1 : 1;
}

static long ssl_info_hash(value v)
{
    return (long) SslInfo_val(v);
}

static void ssl_finalize(value v_info)
{
    SslInfo *info;

    info = SslInfo_val(v_info);
    if(info->fd >= 0) {
        closesocket(info->fd);
        info->fd = -1;
    }
}

/*
 * Pass info in a custom block.
 */
static struct custom_operations win_handle_ops = {
    "ssl_socket",
    ssl_finalize,
    ssl_info_compare,
    ssl_info_hash,
    custom_serialize_default,
    custom_deserialize_default
};

static value ssl_info_new(SOCKET fd)
{
    value v = alloc_custom(&win_handle_ops, sizeof(SslInfo), 0, 1);
    SslInfo *info = SslInfo_val(v);
    info->fd = fd;
    return v;
}

/*
 * Say whether SSL is enabled.
 */
value lm_ssl_enabled(value x)
{
    return Val_false;
}

/*
 * Start the SSL server.
 */
value lm_ssl_init(value x)
{
    return Val_unit;
}

/*
 * Create a new socket.
 */
value lm_ssl_socket(value v_keyfile)
{
    SOCKET s;

    /* Open the socket */
    s = socket(PF_INET, SOCK_STREAM, 0);
    if(s < 0)
        failwith("lm_ssl_socket: socket call failed");

    return ssl_info_new(s);
}

/*
 * Reuse an existing socket.
 */
value lm_ssl_serve(value v_fd, value v_keyfile, value v_dhfile)
{
    SOCKET s = Int_val(v_fd);
    return ssl_info_new(s);
}

/*
 * Get an integer describing the socket.
 */
value lm_ssl_fd(value v_info)
{
    SslInfo *info = SslInfo_val(v_info);
    return Val_int(info->fd);
}

/*
 * Bind the socket to an address.
 */
value lm_ssl_bind(value v_info, value v_addr, value v_port)
{
    SslInfo *info;
    struct sockaddr_in sin;

    /* Get the address */
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = *(uint32 *)v_addr;
    sin.sin_port = htons((short) Int_val(v_port));

    /* Perform the bind */
    info = SslInfo_val(v_info);
    if(bind(info->fd, (struct sockaddr *) &sin, sizeof(sin)) < 0) {
        uerr("lm_ssl_bind");
    }

    return Val_unit;
}

/*
 * Get the address of the socket.
 */
value lm_ssl_get_addr(value v_info)
{
    SslInfo *info;
    struct sockaddr_in sin;
    socklen_t size;
    value a, addr;

    /* Get the address */
    info = SslInfo_val(v_info);
    size = sizeof(sin);
    if(getsockname(info->fd, (struct sockaddr *) &sin, &size) < 0) {
        uerr("lm_ssl_get_addr: getsockname");
    }

    /* Allocate the address */
    a = alloc_inet_addr(sin.sin_addr.s_addr);
    Begin_root(a);
    addr = alloc_small(2, 0);
    Field(addr, 0) = a;
    Field(addr, 1) = Val_int(ntohs(sin.sin_port));
    End_roots();
    return addr;
}

/*
 * Listen.
 */
value lm_ssl_listen(value v_info, value v_dhfile, value v_count)
{
    SslInfo *info;

    info = SslInfo_val(v_info);
    listen(info->fd, Int_val(v_count));
    return Val_unit;
}

/*
 * Accept a connection on the socket.
 */
value lm_ssl_accept(value v_info)
{
    SslInfo *info;
    SOCKET fd;
    int code;

    /* Listen on the server socket */
    info = SslInfo_val(v_info);
    enter_blocking_section();
    fd = accept(info->fd, (struct sockaddr *) 0, 0);
    fflush(stderr);
    leave_blocking_section();
    if(fd < 0) {
        uerr("lm_ssl_accept");
    }

    /* Allocate a new struct */
    return ssl_info_new(fd);
}

/*
 * Make a connection on the socket.
 */
value lm_ssl_connect(value v_info, value v_addr, value v_port)
{
    SslInfo *info;
    struct sockaddr_in sin;
    SOCKET fd;
    int code;

    /* Get the address */
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = *(uint32 *)v_addr;
    sin.sin_port = htons((short) Int_val(v_port));

    /* Make the connection */
    info = SslInfo_val(v_info);
    enter_blocking_section();
    code = connect(info->fd, (struct sockaddr *) &sin, sizeof(sin));
    leave_blocking_section();
    if(code < 0) {
        uerr("lm_ssl_connect");
    }

    return Val_unit;
}

/*
 * Read some data from the connection.
 */
value lm_ssl_read(value v_info, value v_string, value v_off, value v_len)
{
    int off, len, amount;
    SslInfo *info;
    char *buf;

    info = SslInfo_val(v_info);
    buf = String_val(v_string);
    off = Int_val(v_off);
    len = Int_val(v_len);
    enter_blocking_section();
    amount = recv(info->fd, buf + off, len, 0);
    leave_blocking_section();
    return Val_int(amount);
}

/*
 * Write some data to the connection.
 */
value lm_ssl_write(value v_info, value v_string, value v_off, value v_len)
{
    int off, len, amount;
    SslInfo *info;
    char *buf;

    info = SslInfo_val(v_info);
    buf = String_val(v_string);
    off = Int_val(v_off);
    len = Int_val(v_len);
    enter_blocking_section();
    amount = send(info->fd, buf + off, len, 0);
    leave_blocking_section();
    return Val_int(amount);
}

value lm_ssl_flush(value v_info)
{
    return Val_unit;
}

/*
 * Shutdown the connection.
 */
value lm_ssl_shutdown(value v_info)
{
    return Val_unit;
}

/*
 * Close the connection.
 */
value lm_ssl_close(value v_info)
{
    ssl_finalize(v_info);
    return Val_unit;
}

#endif /* !SSL_ENABLED */
