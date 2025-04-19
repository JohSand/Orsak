namespace Xunit

open System.Collections.Generic
open System.Threading.Tasks

#if NET8_0_OR_GREATER

open Xunit.v3

type RepeatAttribute(time: int) =
    inherit DataAttribute()

    override this.GetData(_, _) =
        ValueTask<IReadOnlyCollection<ITheoryDataRow>>(result = [| for i = 1 to time do TheoryDataRow(i) |])

    override this.SupportsDiscoveryEnumeration() = true
#else
open Xunit
open Xunit.Sdk

type RepeatAttribute(time: int) =
    inherit DataAttribute()

    override this.GetData(_) = seq {
        [| for i = 1 to time do box i |]
    }


#endif
