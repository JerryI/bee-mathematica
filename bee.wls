PacletInstall["https://github.com/JerryI/tcp-mathematica/blob/main/JTP.paclet?raw=true", ForceVersionInstall -> True]
<< JTP`

swarm = (JTPClient @@ (URLFetch["http://swarm.polesite.ru/ip.wsp"] // Uncompress)) // JTPClientStart;

Ping[id_]:=With[{uid = id},  
                Print["🏐   from server"];
                JTPClientSend[swarm, Pong[uid]];
            ];

Ja:=With[{}, $connected = True];

$connected = False; 

RegMe:=With[{$KernelName = predictionList[bandSP, 1, 2, 6]//First, $KernelInfo = <|"system"->$System, "geo"->GeoNearest["City", Here], "machine"->$MachineName|>},
            
            logWrite["✏ Trying to register"];
            

            With[{str = TimeConstrained[JTPClientEvaluate[swarm, RegisterMe[$KernelName, $KernelInfo]], 10]//ToString},
                    Print["- "<>str];
                    If[StringContainsQ[str,"hello"],
                        $connected = True;
                        Print["✔   succesfully connected 😊"];
                        JTPClientStartListening[swarm(*, Promise->(Print[#2]&)*)];
                    ,
                        Print["❌   error. Odd answer"];
                    ];
            ]                
                
        ];


cronAdd@<|
    "period" -> Quantity[5,"Seconds"],
    "onetime" -> True,
    "action":>RegMe
|>;

cronAdd@<|
    "period" -> Quantity[30,"Seconds"],
    "action":>With[{},
               If[$connected,
                    (*SendAsync[AliveQ, $cli//First, Promise -> Function[{message,soc},
                        If[TrueQ[message == Hold[Ja]], $connected = True];
                    ]];*)
                    (*cant change the promise, because of the data-racing. Need the message id to track*)

                    JTPClientSend[swarm, AliveQ];
                    $connected = False;
               ,
                    Print["😢 lost"];

                    JTPClientStop[swarm];
                    logWrite["⏳ Trying to recover in 15 sec..."];

                    cronAdd@<|
                        "period" -> Quantity[15,"Seconds"],
                        "onetime" -> True,
                        "action":>With[{}, swarm = (JTPClient @@ (URLFetch["http://swarm.polesite.ru/ip.wsp"] // Uncompress)) // JTPClientStart; RegMe;]
                    |>;

               ];
                
             ]
|>;

cronAdd@<|
    "period" -> Quantity[5,"Seconds"],
    "action":>While[!swarm["log"]["EmptyQ"],(swarm["log"]["Pop"] >>> "server.log")]
|>;



cronAdd@<|
    "period" -> Quantity[10,"Minutes"],
    "action":>Print[Now//DateString]
|>;