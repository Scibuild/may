open! Core
open! Async

let%expect_test "Full end to end tests of the qbe compiler" =
  let sources_dir = "end_to_end/" in
  let%bind may_sources = Async_unix.Sys.readdir sources_dir in
  Array.sort may_sources ~compare:String.compare;
  let%bind () =
    Expect_test_helpers_async.with_temp_dir (fun temp_dir ->
      Deferred.Array.iter may_sources ~how:`Sequential ~f:(fun source_file ->
        let compiler_flags =
          match String.is_suffix source_file ~suffix:".may" with
          | true -> Ok []
          | false ->
            (match String.is_suffix source_file ~suffix:".mayo" with
             | true -> Ok [ "-ownership" ]
             | false -> Error ())
        in
        match compiler_flags with
        | Error () -> return ()
        | Ok compiler_flags ->
          let qbe_file_name = temp_dir ^ "/" ^ source_file ^ ".qbe" in
          let asm_file_name = temp_dir ^ "/" ^ source_file ^ ".s" in
          let exe_file_name = temp_dir ^ "/" ^ source_file ^ ".exe" in
          print_endline "=======================";
          print_endline source_file;
          print_endline "-----------------------";
          let%bind may_proc =
            Process.create_exn
              ~prog:"../bin/may.exe"
              ~args:(compiler_flags @ [ sources_dir ^ source_file; "-out"; qbe_file_name ])
              ()
          in
          let%bind may_err = Reader.contents (Process.stdout may_proc) in
          (match%bind Process.wait may_proc with
           | Error _ ->
             print_endline "ERROR IN MAY COMPILER";
             print_endline "-----------------------";
             print_endline may_err;
             return ()
           | Ok () ->
             let%bind qbe_proc =
               Process.create_exn
                 ~prog:"qbe"
                 ~args:[ qbe_file_name; "-o"; asm_file_name ]
                 ()
             in
             let%bind qbe_err = Reader.contents (Process.stderr qbe_proc) in
             (match%bind Process.wait qbe_proc with
              | Error _ ->
                print_endline "ERROR IN GCC";
                print_endline "-----------------------";
                print_endline qbe_err;
                return ()
              | Ok () ->
                let%bind gcc_proc =
                  Process.create_exn
                    ~prog:"gcc"
                    ~args:
                      [ asm_file_name
                      ; "-o"
                      ; exe_file_name
                      ; "-L../runtime/"
                      ; "-lmayruntime"
                      ]
                    ()
                in
                let%bind gcc_error = Reader.contents (Process.stderr gcc_proc) in
                (match%bind Process.wait gcc_proc with
                 | Error _ ->
                   print_endline "ERROR IN GCC";
                   print_endline "-----------------------";
                   print_endline gcc_error;
                   return ()
                 | Ok () ->
                   let%bind test_proc =
                     Process.create_exn ~prog:exe_file_name ~args:[] ()
                   in
                   let%bind test_output = Reader.contents (Process.stdout test_proc) in
                   let%bind test_err = Reader.contents (Process.stderr test_proc) in
                   print_string test_output;
                   print_endline "-----------------------";
                   print_string test_err;
                   return ()))))
      |> with_timeout_exn
           ~error:(Error.create_s [%message "hit timeout when running end to end test"])
           Time_ns.Span.second)
  in
  print_endline "=======================";
  [%expect
    {|
    =======================
    evolves1.may
    -----------------------
    true
    42
    -----------------------
    =======================
    evolves_fields.mayo
    -----------------------
    I am A
      and I am X
    I am B
      and I am Y
    -----------------------
    =======================
    evolves_with_ownership.mayo
    -----------------------
    42
    -----------------------
    =======================
    exchange.mayo
    -----------------------
    a.id = 2
    a_array[0].id = 1
    Exchanging ....
    a.id = 1
    a_array[0].id = 2
    -----------------------
    =======================
    factorial.may
    -----------------------
    Hello world!
    Recursive factorial of 5: 120
    Loopy factorial of 5: 120
    Loopy factorial of 15: 1307674368000
    -----------------------
    =======================
    hello_world.may
    -----------------------
    Hello world!
    -----------------------
    =======================
    hello_world_std.may
    -----------------------
    Hello world!
    Hello world!
    -----------------------
    =======================
    imp.mayo
    -----------------------
    l1 := 0;
    l2 := 10;
    l3 := !l1 + !l2
    Type Checking...
    (((l1 := (0 : int) : unit);
    (l2 := (10 : int) : unit) : unit);
    (l3 := ((!l1 : int) + (!l2 : int) : int) : unit) : unit)
    Done!
    -----------------------
    =======================
    interface.may
    -----------------------
    I'm an A!
    I'm an A!
    I'm a B!
    I'm a B!
    I'm a C!
    I'm a C!
    I'm a C!
    I'm a C!
    -----------------------
    =======================
    linked_list.may
    -----------------------
    Sum of 4, 2, 7 and -1: 12
    -----------------------
    =======================
    multiple_evolve.may
    -----------------------
    Evolved once!
    -----------------------
    =======================
    mutual_types.may
    -----------------------
    true
    false
    -----------------------
    =======================
    new_array.may
    -----------------------
    0
    -----------------------
    =======================
    null.may
    -----------------------
    a
    c
    a
    Was not null
    Was null
    c
    Was not null
    -----------------------
    =======================
    odd_even.may
    -----------------------
    true
    false
    -----------------------
    =======================
    scratch.mayo
    -----------------------
    -----------------------
    =======================
    subrange.may
    -----------------------
    3
    4
    -----------------------
    =======================
    subrange_oob.may
    -----------------------
    -----------------------
    Panic! Index out of bounds at 6:31-6:32
    =======================
    |}];
  Deferred.return ()
;;
