(**
---
title: Composition
category: Motivation
categoryindex: 1
index: 2
---
*)
(*** condition: prepare ***)
#I "../src/Orsak/bin/Release/net6.0"
#r "Orsak.dll"

open Orsak
open System.Threading.Tasks
(**

###In the beginning, there was OOP

Let's say you have an application that is architected according to [Functional Core, imperative shell](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell). And all the functional code is easy to test and easy to reuse.
But in the imperative shell, how do we compose the side-effectful code?

If you are coming from an object-oriented background, you might be thinking about SOLID. Certainly that was my background. So I made ``IRepositories`` and ``IClients`` and ``IMessageHandlers``, and injected them into ``IDomainService`` and ``IThisService`` and ``IThatService``,
and all that jazz. And inevitably I ended up with too many constructor-parameters, and interfaces with too many members, and weird code duplication. And at the end of the day I thought to my self "There must be a better way. If this is the Right Way<sup>tm</sup>, why is it so hard?"

In the end, I believe that while this is the least bad way of doing oo-programming, it is not enough.

###Putting the 'fun' in programming

If the I in solid is taken to its natural conclusion, your interfaces end up with a single member. Now, this is not an [original](https://blog.ploeh.dk/2014/03/10/solid-the-next-step-is-functional/) observation, but one I believe merits repeating.
But in OO the object is the natural method of composition, and even with dependency injection creating SRP-conforming objects quickly becomes tedious. But give that [objects are merely a poor man's
closures](http://people.csail.mit.edu/gregs/ll1-discuss-archive-html/msg03277.html), maybe we can unlock reuse and composability some other way?
*)

type IMessageClient =
    abstract member FetchData: string -> Task<string>

type IMessageClientProvider =
    abstract member Client: IMessageClient

module MessageClient =
    let fetchData s =
        Effect.Create(fun (provider: #IMessageClientProvider) -> provider.Client.FetchData s)

type IMessageRepository =
    abstract member StoreData: string -> Task<unit>

type IMessageRepositoryProvider =
    abstract member Repository: IMessageRepository

module MessageRepository =
    let storeData s =
        Effect.Create(fun (provider: #IMessageRepositoryProvider) -> provider.Repository.StoreData s)

let fetchAndStore s = eff {
    let! data = MessageClient.fetchData s
    do! MessageRepository.storeData data
}
(**
If functions are objects with a single method, we can view the function above as some bespoke domain-service, where injecting dependencies is as easy as just calling a function, while maintaining the benefits of testability and customizability of dependency injection.
And in turn, reusing that bespoke service is as easy as calling a function. All dependencies are added and tracked automatically, and are visible in the function signature.

And beyond reuse, it also unlocks aspect-like capabilities, but without any magic. Since the effects only execute when run, they can be safely passed around, and binding them with 'let!' creates natural join points.
*)

let weaver (beforeAdvice: Effect<_, _, _>) (effect: Effect<_, _, _>) (afterAdvice: Effect<_, _, _>) = eff {
    do! beforeAdvice
    let! result = effect
    do! afterAdvice
    return result
}
