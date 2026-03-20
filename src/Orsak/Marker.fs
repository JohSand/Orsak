namespace Orsak

type IProvide<'t> =
    abstract member Effect: 't

