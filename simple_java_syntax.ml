(* This file defines the Abstract Syntax Tree for "Simplified Java", *)
(* a minimalist subset of Java.                                      *)
(* We define only data-types in this file; functions modifying       *)
(* these will appear in separate files.                              *)

(* This module allows to track positions in the source files.   *)
open Localizing

(* Keys (to associate to variables). *)
type s_uniqueId = int

(* Data-types: we only support int, bool, void. *)
type s_type =
  | St_int
  | St_bool
  | St_void

(* Constants. *)
type s_constant =
  | Sc_int of int64
  | Sc_bool of bool

(* Operators. *)
(* Unary operators. *)
type s_unary_op =
  | Su_neg
(* Binary operators. *)
type s_binary_op =
  | Sb_add | Sb_sub | Sb_mul | Sb_div
  | Sb_or
  | Sb_lt

(* A variable *)
type s_var =
    { s_var_name:     string;     (* name *)
      s_var_extent:   extent;     (* position in the source file *)
      s_var_type:     s_type;     (* type *)
      s_var_uniqueId: s_uniqueId; (* key *) }

(* Variable declaration: variable + initializer, if any *)
and s_var_decl = s_var * s_expr_e option

(* A procedure, with no parameter:
 *  - it should not return a value
 *  - it should not take any parameter *)
and s_proc =
    { s_proc_name:   string;     (* name *)
      s_proc_body:   s_block;    (* code of the function *) }

(* A function call *)
and  s_proc_call =
    { s_proc_call_class:  string;        (* class of the function called *)
      s_proc_call_name:   string;        (* its name *) }
      
(* Arithmetic and boolean expressions *)
and s_expr =
  | Se_const of s_constant
  | Se_random of int64 * int64
  | Se_var of s_var
  | Se_unary of s_unary_op * s_expr_e
  | Se_binary of s_binary_op * s_expr_e * s_expr_e

(* Expressions annotated with a position in the source file *)
and s_expr_e = s_expr * extent

(* Instructions (assignment, if, while, etc... *)
and s_command =
  | Sc_assign of s_var * s_expr_e
  | Sc_if of s_expr_e * s_block * s_block
  | Sc_while of s_expr_e * s_block
  | Sc_proc_call of s_proc_call
  | Sc_assert of s_expr_e

(* Instructions annotated with a position in the source file. *)
and s_command_e = s_command * extent

(* A block is a sequence of instructions. *)
and s_block = s_command_e list

(* Declaration of a class member: could be a global variable or a function. *)
and s_declaration =
  | Sd_var of s_var_decl
  | Sd_function of s_proc

(* A class = a name + a sequence of declarations *)
type s_class =
    { s_class_name: string;
      s_class_body: s_declaration list; }

(* A program = a list of classes *)
type s_program = s_class list
