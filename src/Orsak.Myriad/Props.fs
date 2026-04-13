namespace Orsak.Myriad

type IProvide<'t> =
    abstract member Effect: 't

module Runner =
    let createFrom<'a> a =
        { new IProvide<'a> with
            member _.Effect = a
        }
