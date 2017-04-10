namespace FSharp.ReportDSL.Examples

open FSharp.ReportDSL
open FSharp.ReportDSL.Helpers
open FSharp.ReportDSL.Core
open FSharp.ReportDSL.Examples.RegimExample

module RegimeComplianceExample =
    open RegimExample
    type Compliance<'t> = { Plan : 't option; Fact: 't option }

    type OilInfo = { Ro : double option ; Nu : double option }
    type PumpInfo = { Name : string; }
    type FlowInfoCompliance  = { Name : string;  Q : double Compliance}
    type NpsInfoCompliance  = { Name : string;  ActivePumps : (PumpInfo []) Compliance; PIn : double Compliance ; POut : double Compliance ; PCol : double Compliance; }
    type RegimeInfo = {RegimeName : string ; OilInfo : OilInfo; NpsInfos : NpsInfo []; FlowInfos : FlowInfo []  } 
