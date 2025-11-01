(* let () = Lwt_main.run (Lib.REPL.main ()) *)

(* First, open the required modules *)
open Core
open Ascii_table

(* Define a record type for your rows *)
type person =
  { name : string
  ; salary  : float
  ; role : string
  }

type department =
  { employees: person list
  ; name: string }

type company =
  { departments: department list
  ; name: string }

(* Create a list of rows *)
let people : person list =
  [ { name = "Magueta"; salary = 1400.0; role = "DBA" }
  ; { name = "Nathan"; salary = 1200.0; role = "FrontEnd Master" }
  ; { name = "Lemos"; salary = 1520.0; role = "Manager" }
  ; { name = "Marinho"; salary = 1244.0; role = "Specialist" }
  ]

let department: department list =
  [ { employees = people
    ; name = "Sales" }
  ; { employees = people
    ; name = "IT" }
  ]

let company: company list =
  [ { departments = department
    ; name = "Marcosoft" }
  ; { departments = department
    ; name = "Nathan J. Solutions" }
  ]

let columns : person Column.t list =
  [ Column.create ~align:Align.Left "Name" (fun (p: person) -> p.name)
  ; Column.create ~align:Align.Right "Salary" (fun p -> Float.to_string p.salary)
  ; Column.create ~align:Align.Left "Role" (fun p -> p.role)
  ]

let columns_2 : department Column.t list =
  [ Column.create ~align:Align.Right "Employees" (fun (d: department) ->
        Ascii_table.to_string ~bars:`Unicode columns d.employees)
  ; Column.create ~align:Align.Right "Department Name" (fun (d: department) -> d.name)
  ]

let columns_3 : company Column.t list =
  [ Column.create ~align:Align.Right "Departments" (fun (c: company) ->
      Ascii_table.to_string ~bars:`Unicode columns_2 c.departments)
  ; Column.create ~align:Align.Right "Name" (fun (c: company) -> c.name)
  ]
let () =
  Ascii_table.to_string ~bars:`Unicode columns_3 company
  |> print_endline
