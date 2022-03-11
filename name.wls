decamel[str_] := 
  StringTrim[
   StringJoin[
    StringSplit[
     str, {RegularExpression["([a-z])([A-Z])"] -> "$1 $2", 
      RegularExpression["([0-9])([A-Z])"] -> "$1 $2", 
      RegularExpression["([a-z])([0-9])"] -> "$1 $2"}]]];
predictionList[func_, num_, min_, max_, decam_ : True] := 
  If[decam == True, 
   decamel /@ 
    Table[func["", "RandomNextElement" -> RandomInteger[{min, max}]], 
     num], Table[
    func["", "RandomNextElement" -> RandomInteger[{min, max}]], num]];
nameGenerator[domain_String, extractor_ : "SegmentedWords"] := 
  Block[{rand}, 
   rand = CommonName[DeleteMissing[RandomEntity[domain, 500]]];
   SequencePredict[rand, FeatureExtractor -> extractor]];

nameGenerator[entOrString_List, extractor_ : "SegmentedWords"] := 
  Block[{names}, 
   With[{heads = DeleteDuplicates[Head /@ entOrString]}, 
    Which[heads === {Entity}, 
     names = CommonName[DeleteMissing[entOrString]];
     SequencePredict[names, FeatureExtractor -> extractor], 
     heads === {String}, 
     names = StringTrim /@ DeleteMissing[entOrString];
     SequencePredict[names, FeatureExtractor -> extractor]]]];
bandSP = nameGenerator[
   EntityClass["MusicAct", "Country" -> Entity["Country", "Germany"]] //
     EntityList];


