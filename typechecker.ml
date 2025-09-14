open Kawa

exception Error of string
let error s = raise (Error s)
let type_error ty_actual ty_expected =
  error (Printf.sprintf "expected %s, got %s"
           (typ_to_string ty_expected) (typ_to_string ty_actual))

module Env = Map.Make(String)
type tenv = typ Env.t

let add_env l tenv =
  List.fold_left (fun env (x, _, t) -> Env.add x t env) tenv l

let typecheck_prog p =
  let tenv = add_env p.globals Env.empty in

  let rec check e typ tenv =
    let typ_e = type_expr e tenv in
    if typ_e <> typ then type_error typ_e typ

  and type_expr e tenv = 
    match e with
    | Int _  -> TInt
    | Bool _ -> TBool
    | Unop (op, e) -> 
        let t = type_expr e tenv in
        (match op, t with
         | Opp, TInt -> TInt
         | Not, TBool -> TBool
         | _ -> error "Opérateur unaire invalide")

    | Binop (op, e1, e2) ->
        let t1 = type_expr e1 tenv in
        let t2 = type_expr e2 tenv in
        (match op, t1, t2 with
         | Add, TInt, TInt | Sub, TInt, TInt | Mul, TInt, TInt | Div, TInt, TInt | Rem, TInt, TInt -> TInt
         | Lt, TInt, TInt | Le, TInt, TInt | Eq, TInt, TInt | Eq, TBool, TBool | And, TBool, TBool | Or, TBool, TBool | Neq, TBool, TBool -> TBool
         | _ -> error "Opérateur binaire invalide")

    | Get m ->
     type_mem_access m tenv
    | New s -> 
        if List.exists (fun cls -> cls.class_name = s) p.classes then 
          TClass s
        else
          error ("Classe inconnue : " ^ s)

    | NewCstr (c, args) -> 
        Printf.printf "Dans le newcstr \n";
        let cls = List.find (fun cls -> cls.class_name = c) p.classes in
        let con = List.find (fun m -> m.method_name = "constructor") cls.methods in
        if List.length args <> List.length con.params then
          error (Printf.sprintf
              "Le nombre d'arguments du constructeur est invalide : attendu %d, fourni %d"
              (List.length con.params)
              (List.length args));
        List.iter2 (fun e (_, t) -> check e t tenv) args con.params;
        TClass c

    | MethCall (e, m, args) ->
        let t = type_expr e tenv in
        (match t with
         | TClass c ->
             let cls = List.find (fun cls1 -> cls1.class_name = c) p.classes in
             let met = List.find (fun meth -> meth.method_name = m) cls.methods in
             if List.length args <> List.length met.params then
               error (Printf.sprintf
                   "Le nombre d'arguments de la méthode est invalide : attendu %d, fourni %d"
                   (List.length met.params)
                   (List.length args));
             List.iter2 (fun e (_, t) -> check e t tenv) args met.params;
             met.return
         | _ -> error ("Type classe attendu, mais " ^ typ_to_string t ^ " a été trouvé"))

    | This -> 
        (try Env.find "this" tenv
         with Not_found -> error "`this` n'est pas défini")

    | CreationTab (t, sizes) ->
        List.iter (fun dim -> check dim TInt tenv) sizes;
        (match t with
           | TInt  -> TArray TInt
           | TBool -> TArray TBool
           | TArray t -> TArray t
           | TVoid  -> error "Type tableau invalide : TVoid"
           | _ -> error ("Type tableau invalide : " ^ typ_to_string t))
  
    | CreationTabInit seq -> 
        let first_type = type_expr (List.hd seq) tenv in
        List.iter (fun e ->
            let t = type_expr e tenv in
            if t <> first_type then
              error "Tous les éléments du tableau doivent être du même type"
          ) seq;
          TArray first_type

  (*fonction qui verifie le bon typage des varaible globales initialisée*)
  and check_var tenv =
      List.iter (fun (x, e_opt, t) ->
        match e_opt with
        | Some e -> 
            Printf.printf "ici\n";
            let typ_e = type_expr e tenv in
            if typ_e <> t then
              type_error typ_e t
        | None -> ()
      ) p.globals

  and type_mem_access m tenv = match m with
    | Var x -> 
        (try Env.find x tenv
         with Not_found -> error ("Variable non trouvée : " ^ x))
    | Field (e, x) -> 
        let t = type_expr e tenv in
        (match t with 
         | TClass c ->
             let cls = List.find (fun cl -> cl.class_name = c) p.classes in 
             (try let  _ , _, typ = List.find (fun(name,_, _) -> name = x ) cls.attributes in
              typ 
              with Not_found -> error ("Champ non défini : " ^ x ^ " dans la classe " ^ c))
         | _ -> error ("Type classe attendu, mais " ^ typ_to_string t ^ " a été trouvé"))
    | ArrayAcces (arr, idx) ->
        let arr_type = type_expr (Get arr) tenv in
        let idx_type = type_expr idx tenv in
        if idx_type <> TInt then
          error "L'indice du tableau doit être un entier"
        else
          (match arr_type with
           | TArray t -> t
           | _ -> error "L'accès à un tableau nécessite un type tableau")
  

  and check_instr i ret tenv = match i with
    | Print e -> check e TInt tenv
    | Return e -> check e ret tenv
    | If (cond, seq1, seq2) ->
        check cond TBool tenv;
        check_seq seq1 ret tenv;
        check_seq seq2 ret tenv;
    | While (cond, seq) ->
        check cond TBool tenv;
        check_seq seq ret tenv;
    | Expr e ->  check e TVoid tenv
    | Set (m, e) ->
        let t = type_mem_access m tenv in
        check e t tenv
  
  and check_seq s ret tenv =
    List.iter (fun i -> check_instr i ret tenv) s

  in

  check_var tenv;
  check_seq p.main TVoid tenv