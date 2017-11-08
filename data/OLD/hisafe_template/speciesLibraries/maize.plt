F: nom plante                                                                            
         NO:                                                         codeplante          mai
         O: monocot ou dicot                               2                             
                   M1: monocotyledone                      0                             
                   M2: dicotyledone                        0                             
                                                                     codemonocot         1
F: développement                                                                         
         NO:                                                         tdmin               6
         NO:                                                         tdmax               28
         O: température pilote                             2                             
                   M1: air                                 4                             
                   M2: culture                             7                             
                                                                     codetemp            2
                   O: échelle.de.temps                     2
                   M11: échelle.journalière                0 
                   M12: échelle.horaire                    0 
                                                                     codegdh             1
                   M2:                                               coeflevamf          1.44
                   M2:                                               coefamflax          1.07
                   M2:                                               coeflaxsen          1.0
                   M2:                                               coefsenlan          1.1
                   M2:                                               coeflevdrp          1.13
                   M2:                                               coefdrpmat          1.03
                   M2:                                               coefflodrp          1.0
         O: plante photopériodique                         2                             
                   M1: oui                                 2                             
                   M2: non                                 0                             
                                                                     codephot            2
                   M1:                                               phobase             0.
                   M1:                                               phosat              0.00000
         O: effet retard stress                            2                             
                   M1: oui                                 1                             
                   M2: non                                 0                             
                                                                     coderetflo          1
                   M1:                                               stressdev           0.2
         O: besoins en froid                              3
                   M1: non                                0 
                   M2: vernalisation(herbacées)           4 
                   M3: dormance(ligneux)                  9
                                                                     codebfroid          1
                   M2:                                               jvcmini             7.0
                   M2:                                               julvernal           274
                   M2:                                               tfroid              6.5
                   M2:                                               ampfroid            10.0
                   O: calcul.dormance                     3
                             M31: forçage                 1
                             M32: Richardson              0
                             M33: Bidabe                  2 
                                                                     codedormance        3
                             M31:                                    ifindorm            70
                             M33:                                    q10                 3.0
                             M33:                                    idebdorm            300
                   M3:                                               stdordebour         100.0
F: début de végétation                                                                                 
         O: annuelle ou pérenne                            2
                   M1: annuelle                            15 
                   M2: pérenne                             0 
                                                                     codeperenne         1
                   O: germination.ou.démarrage             2                             
                            M11: oui                       2                             
                            M12: non                       0                             
                                                                     codegermin          1
                            M11:                                     tgmin               8
                            M11:                                     stpltger            35
                   O: croissance.plantule                  2                             
                            M21: croissance.hypocotyle     5                             
                            M22: plantation                2                             
                                                                     codehypo            1
                            M21:                                     belong              0.022
                            M21:                                     celong              2.04
                            M21:                                     elmax               8
                            M21:                                     nlevlim1            10
                            M21:                                     nlevlim2            50
                            M22:                                     laiplantule         0.00000
                            M22:                                     nbfeuilplant        0
F: feuillage
         NO:                                                         plastochrone        70.0
         NO:                                                         bdens               5
         NO:                                                         laicomp             0.3
         NO:                                                         hautbase            0.00000
         NO:                                                         hautmax             2.5
         O: fonction foliaire                             2
                   M1: LAI                                20 
                   M2: taux.de.recouvrement               4
                                                                     codelaitr           1
                   M1:                                               vlaimax             2.2
                   M1:                                               pentlaimax          5.5
                   M1:                                               udlaimax            3.0
                   M1:                                               ratiodurvieI        0.8
                   M1:                                               tcmin               8
                   M1:                                               tcmax               42
                   M1:                                               ratiosen            0.8
                   M1:                                               abscission          0.0
                   M1:                                               parazofmorte        13.0
                   M1:                                               innturgmin          0.3
                   O: option.calcul.LAI                   2
                             M11: LAInet.direct           2 
                             M12: LAInet=LAIbrut-senes    4
                                                                     codlainet           2
                             M11:                                    dlaimax             1.6e-3
                             M11:                                    tustressmin         0.7
                             M12:                                    dlaimaxbrut         1.6e-3
                             M12:                                    durviesupmax        0.4
                             M12:                                    innsen              0.30
                             M12:                                    rapsenturg          0.5
                   M2:                                               tauxrecouvmax       1.0
                   M2:                                               tauxrecouvkmax      1.0
                   M2:                                               pentrecouv          6.3
                   M2:                                               infrecouv           0.85
F: interception du rayonnement                                                           
         O: interception du rayonnement                    2                             
                   M1: loi.de.Beer                         1                             
                   M2: transferts                          6                             
                                                                     codetransrad        1
                   M1:                                               extin               0.7
                   M2:                                               ktrou               1
                   M2:                                               forme               1
                   M2:                                               rapforme            4
                   M2:                                               adfol               1.0
                   M2:                                               dfolbas             5.0
                   M2:                                               dfolhaut            5.0
F: croissance en biomasse                                                                
         O: seuils de températures                         2                             
                   M1: idem.LAI                            0                             
                   M2: différents.LAI                      2                             
                                                                     codtefcroi          1
                   M2:                                               temin                8
                   M2:                                               temax               40.00
         NO:                                                         teopt               21
         NO:                                                         teoptbis            26
         NO:                                                         efcroijuv           1.9
         NO:                                                         efcroiveg           3.8
         NO:                                                         efcroirepro         3.8
         NO:                                                         remobres            0.2
         NO:                                                         coefmshaut          0.
F: repartition entre organes
         NO:                                                         slamax              250
         NO:                                                         slamin              180
         NO:                                                         tigefeuil           1.0
         NO:                                                         envfruit            0.10
         NO:                                                         sea                 100.0
F: croissance et rendement                                                              
         O: type  de croissance                            2                             
                   M1: déterminée                          11                            
                   M2: indéterminée                        16                             
                                                                     codeindetermin      1
                   M1:                                               nbjgrains           20
                   M1:                                               cgrain              0.0311
                   M1:                                               cgrainv0            0.111
                   M1:                                               nbgrmin             1500
                   O: unité.IR                             2                             
                             M11: jours                    2                             
                             M12: degré.jours              1                             
                                                                     codeir              1
                             M11:                                    vitircarb           0.0103
                             M11:                                    irmax               .53000
                             M12:                                    vitircarbT          1.35e-3
                   M2:                                               nboite              10
                   M2:                                               allocamx            .60000
                   M2:                                               afpf                .40000
                   M2:                                               bfpf                4.32000
                   M2:                                               sdrpnou             300.0
                   M2:                                               spfrmin             0.75
                   M2:                                               spfrmax             1.0
                   M2:                                               splaimin            0.2
                   M2:                                               splaimax            1.0
                   O: nombre.inflorescences               2
                             M21: imposé                  1 
                             M22: fonction.état.trophique 2
                                                                     codcalinflo         2
                             M21:                                    nbinflo             2.0
                             M22:                                    inflomax            5.0
                             M22:                                    pentinflores        0.8
         O: contrainte thermique remplissage               2                             
                   M1: oui                                 2                             
                   M2: non                                 0                             
                                                                     codetremp           2
                   M1:                                               tminremp            .00000
                   M1:                                               tmaxremp            32.0
         NO:                                                         vitpropsucre        0.0000
         NO:                                                         vitprophuile        0.0000
         NO:                                                         vitirazo            9.24e-3
F: racines                                                                               
         NO:                                                         sensanox            0.0
         NO:                                                         stoprac             sen
         NO:                                                         sensrsec            0.0
         NO:                                                         contrdamax          0.3
         O: température pilote                             2                             
                   M1: culture                             0                             
                   M2: sol.(seuil.TGMIN)                   0                             
                                                                     codetemprac         2
         O: densité racinaire                              2                             
                   M1: profil.optimal.type                 3                             
                   M2: densité.vraie                       4                             
                                                                     coderacine          2
                   M1:                                               zlabour             25
                   M1:                                               zpente              110
                   M1:                                               zprlim              140
                   M2:                                               draclong            250.0
                   M2:                                               debsenrac           1500.00
                   M2:                                               lvfront             5e-2
                   M2:                                               longsperac          11e-3
F: gel
         NO:                                                         tletale             -5.0
         NO:                                                         tdebgel             0.0
         O: gel plantule ou levée                         2                                           
                   M1: non                                0 
                   M2: oui                                3
                                                                     codgellev           2
                   M2:                                               nbfgellev           3
                   M2:                                               tgellev10           -1.0
                   M2:                                               tgellev90           -4.0
         O: gel feuillage phase juvénile (jusqu'à AMF)    2                                           
                   M1: non                                0 
                   M2: oui                                2
                                                                     codgeljuv           2
                   M2:                                               tgeljuv10           -1.0
                   M2:                                               tgeljuv90           -4.0
         O: gel feuillage phase adulte                    2                                           
                   M1: non                                0 
                   M2: oui                                2
                                                                     codgelveg           2
                   M2:                                               tgelveg10           -1.0
                   M2:                                               tgelveg90           -4.0
         O: gel fleurs/fruits (à partir de FLO)           2                                           
                   M1: non                                0 
                   M2: oui                                2
                                                                     codgelflo           2
                   M2:                                               tgelflo10           0
                   M2:                                               tgelflo90           -1
F: eau                                                                                   
         NO:                                                         psisto               12
         NO:                                                         psiturg             5
         NO:                                                         h2ofeuilverte       0.90
         NO:                                                         h2ofeuiljaune       0.15
         NO:                                                         h2otigestruc        0.60
         NO:                                                         h2oreserve          0.70
         NO:                                                         h2ofrvert           0.40
         NO:                                                         stdrpdes            650.0
         NO:                                                         deshydbase          0.008
         NO:                                                         tempdeshyd          0.005
         O: besoins en eau                                 2                             
                   M1: coefficient.cultural                1                             
                   M2: modèle.résistif                     1                             
                                                                     codebeso            1
                   M1:                                               kmax                1.15
                   M2:                                               rsmin               215.0
         O: interception de la pluie                       2                             
                   M1: oui                                 3                             
                   M2: non                                 0                             
                                                                     codeintercept       2
                   M1:                                               mouillabil             .00000
                   M1:                                               stemflowmax            .00000
                   M1:                                               kstemflow              .00000
F: azote                                                                                 
         NO:                                                         Vmax1                  .00180
         NO:                                                         Kmabs1               50.00
         NO:                                                         Vmax2                  .017
         NO:                                                         Kmabs2               25000.00
         NO:                                                         adil                3.5
         NO:                                                         bdil                0.37
         NO:                                                         adilmax             5.44
         NO:                                                         bdilmax             0.37
         NO:                                                         masecNmax           1.0
         NO:                                                         INNmin              0.3
         NO:                                                         inngrain1           0.7
         NO:                                                         inngrain2           1.0
         O: legumineuse                                    2                             
                   M1: non                                 0                             
                   M2: oui                                 18                             
                                                                     codelegume          1
                   O: fixation.symbiotique                 2
                             M11: azote.critique           0 
                             M12: activite.nodosite        13
                                                                     codesymbiose        2
                             M12:                                    stlevdno            150.0
                             M12:                                    stdnofno            500.0
                             M12:                                    stfnofvino          300.0
                             M12:                                    vitno               0.003
                             M12:                                    profnod             40.0
                             M12:                                    concNnodseuil       13.0e-1
                             M12:                                    concNrac0           1.2
                             M12:                                    concNrac100         0.4
                             M12:                                    hunod               1.5
                             M12:                                    tempnod1            0.0
                             M12:                                    tempnod2            30.0
                             M12:                                    tempnod3            36.0
                             M12:                                    tempnod4            50.0
         O: effet azote sur nb fruits                      2                             
                   M1: non                                 0                             
                   M2: oui(inns)                           0                             
                                                                     codazofruit         2
F: variétal                                                                              
         TV: les différentes variétés                      12
                   M01: DK250                              22 
                   M02: Pactol                             22 
                   M03: Cherif                             22 
                   M04: Furio                              22 
                   M05: Dunia                              22 
                   M06: Volga                              22 
                   M07: Cecilia                            22 
                   M08: DK604                              22 
                   M09: Nobilis-DEA                        22 
                   M10: DK300                              22 
                   M11: Anjou285                           22 
                   M12: DK312                              22 
                                                                     codevar             DK250
                   M01:                                              stlevamf            225
                   M01:                                              stamflax            450
                   M01:                                              stlevdrp            995
                   M01:                                              pgrainmaxi          0.313
                   M01:                                              adens               -0.12
                   M01:                                              croirac             0.15
                   M01:                                              stflodrp            90
                   M01:                                              durvieF             240
                   O:  codebfroid                          1
                             M012:                                   jvc                 0
                   O:  codephot                            1
                             M011:                                   sensiphot           0
                   O:  codlainet                           2
                             M011:                                   stlaxsen            688
                             M011:                                   stsenlan            272
                   O:  codeindetermin                      4
                             M011:                                   nbgrmax             4500
                             M011:                                   stdrpmat            640
                             M012:                                   afruitpot           .00000
                             M012:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M012:                                   fixmax              6.0
                                                                     codevar             Pactol
                   M02:                                              stlevamf            253
                   M02:                                              stamflax            507
                   M02:                                              stlevdrp            1080
                   M02:                                              pgrainmaxi          .296
                   M02:                                              adens               -.12000
                   M02:                                              croirac             .15000
                   M02:                                              stflodrp            90
                   M02:                                              durvieF             240
                   O:  codebfroid                          1
                             M022:                                   jvc                 0
                   O:  codephot                            1
                             M021:                                   sensiphot           0
                   O:  codlainet                           2
                             M021:                                   stlaxsen            730.
                             M021:                                   stsenlan            190.
                   O:  codeindetermin                      4
                             M021:                                   nbgrmax             4500
                             M021:                                   stdrpmat            600
                             M022:                                   afruitpot           .00000
                             M022:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M022:                                   fixmax              6.0
                                                                     codevar             Cherif
                   M03:                                              stlevamf            265
                   M03:                                              stamflax            530
                   M03:                                              stlevdrp            1115
                   M03:                                              pgrainmaxi          .33
                   M03:                                              adens               -.12000
                   M03:                                              croirac             .15000
                   M03:                                              stflodrp            90
                   M03:                                              durvieF             240
                   O:  codebfroid                          1
                             M032:                                   jvc                 0
                   O:  codephot                            1
                             M031:                                   sensiphot           0
                   O:  codlainet                           2
                             M031:                                   stlaxsen            748
                             M031:                                   stsenlan            177
                   O:  codeindetermin                      4
                             M031:                                   nbgrmax             4500
                             M031:                                   stdrpmat            605
                             M032:                                   afruitpot           .00000
                             M032:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M032:                                   fixmax              6.0
                                                                     codevar             Furio
                   M04:                                              stlevamf            275
                   M04:                                              stamflax            550
                   M04:                                              stlevdrp            1145
                   M04:                                              pgrainmaxi          .33
                   M04:                                              adens               -.12000
                   M04:                                              croirac             .15000
                   M04:                                              stflodrp            90
                   M04:                                              durvieF             350
                   O:  codebfroid                          1  
                             M042:                                   jvc                 0
                   O:  codephot                            1
                             M041:                                   sensiphot           0
                   O:  codlainet                           2
                             M041:                                   stlaxsen            763
                             M041:                                   stsenlan            197
                   O:  codeindetermin                      4
                             M041:                                   nbgrmax             4500
                             M041:                                   stdrpmat            640
                             M042:                                   afruitpot           .00000
                             M042:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M042:                                   fixmax              6.0
                                                                     codevar             Dunia
                   M05:                                              stlevamf            285
                   M05:                                              stamflax            570
                   M05:                                              stlevdrp            1175
                   M05:                                              pgrainmaxi          .33
                   M05:                                              adens               -.12000
                   M05:                                              croirac             0.15
                   M05:                                              stflodrp            90
                   M05:                                              durvieF             260
                   O:  codebfroid                          1
                             M052:                                   jvc                 0
                   O:  codephot                            1
                             M051:                                   sensiphot           0
                   O:  codlainet                           2
                             M051:                                   stlaxsen            778
                             M051:                                   stsenlan            197
                   O:  codeindetermin                      4
                             M051:                                   nbgrmax             4500
                             M051:                                   stdrpmat            655
                             M052:                                   afruitpot           .00000
                             M052:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M052:                                   fixmax              6.0
                                                                     codevar             Volga
                   M06:                                              stlevamf            288
                   M06:                                              stamflax            577
                   M06:                                              stlevdrp            1185
                   M06:                                              pgrainmaxi          .322
                   M06:                                              adens               -.12
                   M06:                                              croirac             .15000
                   M06:                                              stflodrp            90
                   M06:                                              durvieF             260
                   O:  codebfroid                          1
                             M062:                                   jvc                 0
                   O:  codephot                            1
                             M061:                                   sensiphot           0
                   O:  codlainet                           2
                             M061:                                   stlaxsen            783
                             M061:                                   stsenlan            252
                   O:  codeindetermin                      4
                             M061:                                   nbgrmax             4500
                             M061:                                   stdrpmat            715
                             M062:                                   afruitpot           .00000
                             M062:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M062:                                   fixmax              6.0
                                                                     codevar             Cecilia
                   M07:                                              stlevamf            300
                   M07:                                              stamflax            600
                   M07:                                              stlevdrp            1220
                   M07:                                              pgrainmaxi          .348
                   M07:                                              adens               -.12
                   M07:                                              croirac             .15000
                   M07:                                              stflodrp            90
                   M07:                                              durvieF             260
                   O:  codebfroid                          1
                             M072:                                   jvc                 0
                   O:  codephot                            1
                             M071:                                   sensiphot           0
                   O:  codlainet                           2
                             M071:                                   stlaxsen            800
                             M071:                                   stsenlan            220
                   O:  codeindetermin                      4
                             M071:                                   nbgrmax             4500
                             M071:                                   stdrpmat            700
                             M072:                                   afruitpot           .00000
                             M072:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M072:                                   fixmax              6.0
                                                                     codevar             DK604
                   M08:                                              stlevamf            300
                   M08:                                              stamflax            600
                   M08:                                              stlevdrp            1220
                   M08:                                              pgrainmaxi          .33
                   M08:                                              adens               -.12
                   M08:                                              croirac             .15000
                   M08:                                              stflodrp            90
                   M08:                                              durvieF             260
                   O:  codebfroid                          1
                             M082:                                   jvc                 0
                   O:  codephot                            1
                             M081:                                   sensiphot           0
                   O:  codlainet                           2
                             M081:                                   stlaxsen            800
                             M081:                                   stsenlan            245
                   O:  codeindetermin                      4
                             M081:                                   nbgrmax             4500
                             M081:                                   stdrpmat            725
                             M082:                                   afruitpot           .00000
                             M082:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M082:                                   fixmax              6.0
                                                                     codevar             Nobilis-DEA
                   M09:                                              stlevamf            233
                   M09:                                              stamflax            467
                   M09:                                              stlevdrp            1020
                   M09:                                              pgrainmaxi          .313
                   M09:                                              adens               -.12
                   M09:                                              croirac             .15000
                   M09:                                              stflodrp            90
                   M09:                                              durvieF             260
                   O:  codebfroid                          1
                             M092:                                   jvc                 0
                   O:  codephot                            1
                             M091:                                   sensiphot           0
                   O:  codlainet                           2
                             M091:                                   stlaxsen            700
                             M091:                                   stsenlan            270
                   O:  codeindetermin                      4
                             M091:                                   nbgrmax             4500
                             M091:                                   stdrpmat            650
                             M092:                                   afruitpot           .00000
                             M092:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M092:                                   fixmax              6.0
                                                                     codevar             DK300
                   M10:                                              stlevamf            255
                   M10:                                              stamflax            510
                   M10:                                              stlevdrp            1085
                   M10:                                              pgrainmaxi          .322
                   M10:                                              adens               -.12
                   M10:                                              croirac             .15000
                   M10:                                              stflodrp            90
                   M10:                                              durvieF             260
                   O:  codebfroid                          1
                             M102:                                   jvc                 0
                   O:  codephot                            1
                             M101:                                   sensiphot           0
                   O:  codlainet                           2
                             M101:                                   stlaxsen            738
                             M101:                                   stsenlan            222
                   O:  codeindetermin                      4
                             M101:                                   nbgrmax             4500
                             M101:                                   stdrpmat            640
                             M102:                                   afruitpot           .00000
                             M102:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M102:                                   fixmax              6.0
                                                                     codevar             Anjou285
                   M11:                                              stlevamf            242
                   M11:                                              stamflax            483
                   M11:                                              stlevdrp            1045
                   M11:                                              pgrainmaxi          .26
                   M11:                                              adens               -.12
                   M11:                                              croirac             .15000
                   M11:                                              stflodrp            90
                   M11:                                              durvieF             260
                   O:  codebfroid                          1
                             M112:                                   jvc                 0
                   O:  codephot                            1
                             M111:                                   sensiphot           0
                   O:  codlainet                           2
                             M111:                                   stlaxsen            713
                             M111:                                   stsenlan            192
                   O:  codeindetermin                      4
                             M111:                                   nbgrmax             4500
                             M111:                                   stdrpmat            585
                             M112:                                   afruitpot           .00000
                             M112:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M112:                                   fixmax              6.0
                                                                     codevar             DK312
                   M12:                                              stlevamf            260
                   M12:                                              stamflax            520
                   M12:                                              stlevdrp            1100
                   M12:                                              pgrainmaxi          .313
                   M12:                                              adens               -.12
                   M12:                                              croirac             .15000
                   M12:                                              stflodrp            90
                   M12:                                              durvieF             260
                   O:  codebfroid                          1
                             M122:                                   jvc                 0
                   O:  codephot                            1
                             M121:                                   sensiphot           0
                   O:  codlainet                           2
                             M121:                                   stlaxsen            740
                             M121:                                   stsenlan            200
                   O:  codeindetermin                      4
                             M121:                                   nbgrmax             4500
                             M121:                                   stdrpmat            620
                             M122:                                   afruitpot           .00000
                             M122:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M122:                                   fixmax              6.0