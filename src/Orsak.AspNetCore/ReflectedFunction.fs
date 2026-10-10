/// <exclude/>
/// <summary>
/// Reads a function captured with <c>[&lt;ReflectedDefinition(includeValue = true)&gt;]</c>: its parameter names, the
/// method it refers to, and a name for it.
/// </summary>
module Orsak.AspNetCore.ReflectedFunction

open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

/// <summary>What can be read from the quotation of a function.</summary>
type FunctionShape = {
    /// <summary>
    /// The names of its parameters, in order, with the elements of a tupled parameter as parameters of their own.
    /// Empty when the function isn't a lambda, such as a partial application bound to a name.
    /// </summary>
    ParameterNames: string array
    /// <summary>
    /// The method it refers to, when it is a reference to one: a function or method passed by name, possibly
    /// partially applied. None for a lambda doing more than calling a method with its parameters.
    /// </summary>
    Method: MethodInfo option
    /// <summary>The name of the function, method or value it refers to, if any.</summary>
    Name: string option
}

let private (|TupledParameters|_|) (expr: Expr) =
    // fun (a, b) -> ... is quoted as: fun tupledArg -> let a = tupledArg.Item1 in let b = tupledArg.Item2 in ...
    let rec elements (tuple: Var) (names: string list) (e: Expr) =
        match e with
        | Let(var, TupleGet(Var source, _), body) when source = tuple -> elements tuple (var.Name :: names) body
        | body -> List.rev names, body

    match expr with
    | Lambda(tuple, body) when tuple.Name = "tupledArg" ->
        match elements tuple [] body with
        | [], _ -> None
        | names, body -> Some(names, body)
    | _ -> None

/// The parameter names of the lambdas at the start of `expr`, curried or tupled, and the body after them.
let rec private parameters (names: string list) (expr: Expr) =
    match expr with
    | TupledParameters(tupled, body) -> parameters (List.rev tupled @ names) body
    | Lambda(var, body) -> parameters (var.Name :: names) body
    | body -> List.rev names, body

/// The variables bound by a partial application, `add 1` quoted as `let a = 1 in fun b -> add a b`.
let rec private partialArguments (vars: Var list) (expr: Expr) =
    match expr with
    | Let(var, _, (Let _ | Lambda _ as body)) -> partialArguments (var :: vars) body
    | body -> vars, body

/// <summary>Reads the parameter names, method and name of the function quoted by <paramref name="expr"/>.</summary>
/// <param name="expr">The quotation of the function, without its value</param>
let read (expr: Expr) : FunctionShape =
    let bound, lambda = partialArguments [] expr
    let names, body = parameters [] lambda

    // a reference calls the method with nothing but its parameters, and the partially applied arguments
    let method =
        match body with
        | Call(_, mi, args) when args |> List.forall (function Var v -> List.contains v bound || List.contains v.Name names | _ -> false) ->
            Some mi
        | _ -> None

    let name =
        match method, expr with
        | Some mi, _ -> Some mi.Name
        | None, PropertyGet(_, property, []) -> Some property.Name
        | None, ValueWithName(_, _, name) -> Some name
        | None, Application(ValueWithName(_, _, name), _) -> Some name
        | None, _ -> None

    { ParameterNames = Array.ofList names; Method = method; Name = name }

/// <summary>
/// The value of a function captured with <c>[&lt;ReflectedDefinition(includeValue = true)&gt;]</c>, and what can be
/// read from its quotation.
/// </summary>
/// <param name="captured">The captured function</param>
/// <exception cref="T:System.ArgumentException">The function wasn't captured with its value</exception>
let capture (captured: Expr<'f>) : 'f * FunctionShape =
    match captured with
    | WithValue(:? 'f as value, _, expr) -> value, read expr
    | _ -> raise (ArgumentException "This expression is expected to be constructed with ReflectedDefinition(includeValue = true).")
