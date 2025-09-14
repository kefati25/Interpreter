open Kawa
type value =
  | VInt  of int
  | VBool of bool
  | VObj  of obj
  | VArray of typ * value array
  | Null

and obj = {
  cls:    string;
  fields: (string, value) Hashtbl.t;
}

exception Error of string
exception Return of value


let exec_prog (p: program): unit =
  
  let env = Hashtbl.create 16 in

  (* Initialisation des variables globales *)
  List.iter (fun (x, e1, t) ->
    let value = match e1 with
      | Some e -> 
          let rec eval (e: expr): value = match e with
            | Int n -> VInt n
            | Bool b -> VBool b
            | CreationTab (t, s) ->
              let dims = List.map (fun e ->
                match eval e with
                | VInt n -> n
                | _ -> raise (Error "Les dimensions du tableau doivent être des entiers")
              ) s
            in
              if List.exists (fun dim -> dim < 0) dims then
                raise (Error "Les dimensions du tableau doivent être positives")
              else
              let rec create_array dims = 
                match dims with
                | [] -> Null
                | [dim] -> VArray (t, Array.make dim Null)
                | dim :: rest ->
                  let in_array = create_array rest in
                  VArray (t, Array.init dim (fun _ -> in_array))
                
            in
                create_array dims
            
            | _ -> raise (Error "Expression non supportée pour l'initialisation globale")
          in
          eval e
      | None -> Null 
    in
    Hashtbl.add env x value
  ) p.globals;
 


  (* Fonction qui trouve la class*)
  let rec find_class c_name =
    try  List.find (fun c -> c.class_name = c_name) p.classes
    with Not_found -> failwith("Classe " ^ c_name ^ " introuvable")
  in

  let rec eval_call f this args =
    (* Fonction qui trouve la methode*)
    let rec find_meth c_name =
      let cls = find_class c_name in
      try List.find (fun m -> m.method_name = f ) cls.methods
    with Not_found -> (
      match cls.parent with
      |Some p_name -> find_meth p_name
      | None -> failwith("Methode " ^ f ^ " introuvable dans la classe " ^ c_name)
    )
    in
    

    let this = match this with 
    | VObj o -> o
    | _  -> raise (Error "`this` doit être un objet")
    in

    let meth_def = find_meth this.cls in
    let lenv1 = Hashtbl.create 16 in

    List.iter (fun (x, e1, t) ->
      let value = match e1 with
        | Some e -> 
            let rec eval (e: expr): value = match e with
              | Int n -> VInt n
              | Bool b -> VBool b
              | _ -> raise (Error "Expression non supportée pour l'initialisation globale")
            in
            eval e
        | None -> Null 
      in
      Hashtbl.add lenv1 x value
    ) meth_def.locals;

   
    List.iter2
    (fun (param_name, _) arg ->
     Hashtbl.add lenv1 param_name arg )
    meth_def.params args;
    Hashtbl.add lenv1 "this" (VObj this);

    try
      exec_seq meth_def.code lenv1;
      Null  
    with Return v -> v 
    


  and exec_seq s lenv =
    let rec evali e = match eval e with
      | VInt n -> n
      | _ -> assert false
    and evalb e = match eval e with
      | VBool b -> b
      | _ -> assert false
    and evalo e = match eval e with
      | VObj o -> o
      | _ -> assert false
        
    and eval (e: expr): value = match e with
      | Int n  -> VInt n
      | Bool b -> VBool b
      | Binop (op, e1, e2) ->
          let v1 = eval e1 in
          let v2 = eval e2 in
          (match op, v1, v2 with
            | Add, VInt n1, VInt n2 -> VInt (n1 + n2)
            | Sub, VInt n1, VInt n2 -> VInt (n1 - n2)
            | Mul, VInt n1, VInt n2 -> VInt (n1 * n2)
            | Rem, VInt n1, VInt n2 ->
              if n2 = 0 then raise (Error "Division par zéro") else VInt (n1 mod n2)
            | Eq, VInt n1, VInt n2 -> VBool (n1 = n2)
            | Eq, VBool n1, VBool n2 -> VBool (n1 = n2)
            | Lt, VInt n1, VInt n2 -> VBool (n1 < n2)
            | Le, VInt n1, VInt n2 -> VBool (n1 <= n2) 
            | Neq, VInt n1, VInt n2 -> VBool (n1 <> n2)
            | Div, VInt n1, VInt n2 ->
              if n2 = 0 then raise (Error "Division par zéro") else VInt (n1 / n2)
            | And, VBool b1, VBool b2 -> VBool (b1 && b2)
            | Or, VBool b1, VBool b2 -> VBool (b1 || b2)
            | _ -> raise (Error "Opération binaire invalide"))
      | Unop (op, e) ->
          let v1 = eval e in
          (match op, v1 with
            | Opp, VInt n  -> VInt (-n)
            | Not, VBool b -> VBool (not b)
            | _ -> raise (Error "Opération unitaire invalide"))
      |Get x -> (
        match x with
        | Var var_name ->
            if Hashtbl.mem lenv var_name then Hashtbl.find lenv  var_name
            else if Hashtbl.mem env var_name then Hashtbl.find env var_name
            else raise (Error ("Variable non trouvée : " ^ var_name))
        | Field (obj, field_name) ->
            let obj_value = evalo obj in
            if Hashtbl.mem obj_value.fields field_name then
              Hashtbl.find obj_value.fields field_name
            else raise (Error ("Field non trouvée : " ^ field_name))
        | ArrayAcces (id, i) ->
          let index = evali i in
          let arr = eval (Get id) in
          (match arr with
            | VArray(_, arr) -> 
              if index < 0 || index >= Array.length arr then
                raise (Error "Indice hors du tableau")
              else
                arr.(index)
            | _ -> raise (Error "La variable n'est pas un tableau"))
      )
      | New id -> 
        let new_obj = {cls = id ; fields = Hashtbl.create 16} in
        let cls = find_class id in

        List.iter (fun (at_name, e1, _) ->
          let value = match e1 with
            |Some e-> eval e
            |None -> Null
        in
        Hashtbl.add new_obj.fields at_name value) cls.attributes;

        (match cls.parent with
          | Some p_name ->
              let p = find_class p_name in
               List.iter (fun (at_name, e1, _) ->
              let value = match e1 with
                |Some e-> eval e
                |None -> Null
              in
              Hashtbl.add new_obj.fields at_name value) p.attributes;

          | None -> ());
        VObj new_obj

        | NewCstr (id, args) ->
          let new_obj = { cls = id; fields = Hashtbl.create 16 } in
          let cls = find_class id in
          
          List.iter (fun (at_name, e1, _) ->
            let value = match e1 with
              |Some e-> eval e
              |None -> Null
          in
          Hashtbl.add new_obj.fields at_name value) cls.attributes;
          
          (match cls.parent with
           | Some p_name ->
              let p = find_class p_name in
              List.iter (fun (at_name, e1, _) ->
                let value = match e1 with
                  |Some e-> eval e
                  |None -> Null
              in
              Hashtbl.add new_obj.fields at_name value) p.attributes;
           | None -> ());
        
          let eval_args = List.map (fun e -> eval e) args in
          let _ = eval_call "constructor" (VObj new_obj) eval_args in
          VObj new_obj

      | MethCall (obj, meth_name, seq) -> 
        let o = eval obj in
        let eval_args = List.map (fun e -> eval e) seq in
        eval_call meth_name o eval_args

      | This -> (try Hashtbl.find lenv "this" with Not_found -> raise(Error "This n'est pas indentifiée"))
      | CreationTab (t, s) ->
        let dims = List.map evali s in
        if List.exists (fun dim -> dim < 0) dims then
          raise (Error "Les dimensions du tableau doivent être positives")
        else
        let rec create_array dims = 
          match dims with
          | [] -> Null
          | [dim] -> VArray (t, Array.make dim Null)
          | dim :: rest ->
            let in_array = create_array rest in
            VArray (t, Array.init dim (fun _ -> in_array))
          
      in
          create_array dims 
      | CreationTabInit seq ->
        let v = List.map eval seq in
        let t =
          if  List.length v = 0 then
            TArray TVoid
          else
            match List.hd v with
        | VInt _ -> TInt
        | VBool _ -> TBool
        | VObj c -> TClass c.cls
        | VArray (t, _) -> TArray t
        | Null -> TVoid
      in
      let arr = Array.of_list v in
      VArray (t, arr)  

    in

    let rec exec (i: instr): unit = match i with
      | Print e -> 
         let v = eval e in
         (match v with
           | VInt n -> Printf.printf "%d\n" n
           | VBool b -> Printf.printf "%b\n" b
           | VObj o -> Printf.printf "[Object of class %s]\n" o.cls
           | _ -> raise (Error "Type d'argument invalide pour print"))
      | Set (m, e) -> 
          let v = eval e in

          (match m with
            |Var x-> (

              if Hashtbl.mem lenv x then Hashtbl.replace lenv  x v
              else if Hashtbl.mem env x then Hashtbl.replace env x v
              else raise (Error ("Variable non trouvée : " ^ x))

            )
            | Field  (e, name)-> 
                let o = eval e in
                (match o with
                |VObj obj -> Hashtbl.replace obj.fields name v
                | _ -> raise (Error "Expected an object")
            )
            | ArrayAcces (id, i)->              
              let index = evali i in
              let arr = eval (Get id) in
              (match arr with
              | VArray(_, arr) -> 
                  
                  if index < 0 || index >= Array.length arr then
                    raise (Error "Indice hors du tableau")
                  else
                     arr.(index) <- v;
                    
            | _ -> raise (Error "La variable n'est pas un tableau"))
          )
      | If ( e, seq1, seq2) -> (
        let condition = eval e in
        match condition with
              | VBool true -> exec_seq seq1
              | VBool false -> exec_seq seq2 
              | _ -> raise (Error "La condition doit être boolean")
      )
      | While (e, seq) ->
        let rec loop () =
          let condition = eval e in
          match condition with
          | VBool true -> 
              exec_seq seq ;  
              loop ()             
          | VBool false -> ()     
          | _ -> raise (Error "La condition doit être boolean")
        in
        loop ()
      |Expr e -> 
        let _ = eval e in
        ()
      | Return e ->
        let va = eval e in
        raise (Return va)
    
    and exec_seq s = 
      List.iter exec s
    in

    exec_seq s
  in

  if p.main = [] then
    Printf.printf "Aucune instruction dans le programme principal\n"
  else
    exec_seq p.main (Hashtbl.create 1)