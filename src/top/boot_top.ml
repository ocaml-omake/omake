

let () = 
  begin
    Clflags.include_dirs := Top_config.include_dirs @ !Clflags.include_dirs ;
  end
