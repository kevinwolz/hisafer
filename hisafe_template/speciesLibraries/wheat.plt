F: nom plante                                                                            
         NO:                                                         codeplante          ble
         O: monocot ou dicot                               2                             
                   M1: monocotyledone                      0                             
                   M2: dicotyledone                        0                             
                                                                     codemonocot         1
F: développement                                                                         
         NO:                                                         tdmin                  .00000
         NO:                                                         tdmax                28.00000
         O: température pilote                             2                             
                   M1: air                                 4                             
                   M2: culture                             7                             
                                                                     codetemp            2
                   O: échelle.de.temps                     2
                   M11: échelle.journalière                0 
                   M12: échelle.horaire                    0 
                                                                     codegdh             1
                   M2:                                               coeflevamf          1.0
                   M2:                                               coefamflax          1.0
                   M2:                                               coeflaxsen          1.0
                   M2:                                               coefsenlan          1.0
                   M2:                                               coeflevdrp          1.0
                   M2:                                               coefdrpmat          1.0
                   M2:                                               coefflodrp          1.0
         O: plante photopériodique                         2                             
                   M1: oui                                 2                             
                   M2: non                                 0                             
                                                                     codephot            1
                   M1:                                               phobase             6.3
                   M1:                                               phosat              20.00000
         O: effet retard stress                            2                             
                   M1: oui                                 1                             
                   M2: non                                 0                             
                                                                     coderetflo          1
                   M1:                                               stressdev           0.2
         O: besoins en froid                              3
                   M1: non                                0 
                   M2: vernalisation(herbacées)           4 
                   M3: dormance(ligneux)                  9
                                                                     codebfroid          2
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
                            M11:                                     tgmin               0.00000
                            M11:                                     stpltger            50.00000
                   O: croissance.plantule                  2                             
                            M21: croissance.hypocotyle     5                             
                            M22: plantation                2                             
                                                                     codehypo            1
                            M21:                                     belong              0.01200
                            M21:                                     celong              3.20000
                            M21:                                     elmax               8.00000
                            M21:                                     nlevlim1            10
                            M21:                                     nlevlim2            50
                            M22:                                     laiplantule         0.00000
                            M22:                                     nbfeuilplant        0
F: feuillage
         NO:                                                         plastochrone        120.0
         NO:                                                         bdens               7.00000
         NO:                                                         laicomp             0.304
         NO:                                                         hautbase            0.00000
         NO:                                                         hautmax             1.20
         O: fonction foliaire                             2
                   M1: LAI                                20 
                   M2: taux.de.recouvrement               4
                                                                     codelaitr           1
                   M1:                                               vlaimax             2.2
                   M1:                                               pentlaimax          5.5
                   M1:                                               udlaimax            3.0
                   M1:                                               ratiodurvieI        0.8
                   M1:                                               tcmin               0
                   M1:                                               tcmax               40.00000
                   M1:                                               ratiosen            0.8
                   M1:                                               abscission          0.0
                   M1:                                               parazofmorte        13.0
                   M1:                                               innturgmin          -0.2
                   O: option.calcul.LAI                   2
                             M11: LAInet.direct           2 
                             M12: LAInet=LAIbrut-senes    4
                                                                     codlainet           2
                             M11:                                    dlaimax             4.4e-4
                             M11:                                    tustressmin         0.7
                             M12:                                    dlaimaxbrut         4.4e-4
                             M12:                                    durviesupmax        0.4
                             M12:                                    innsen              0.35
                             M12:                                    rapsenturg          0.5
                   M2:                                               tauxrecouvmax       1.0
                   M2:                                               tauxrecouvkmax      1.0
                   M2:                                               pentrecouv          4.5
                   M2:                                               infrecouv           0.85
F: interception du rayonnement                                                           
         O: interception du rayonnement                    2                             
                   M1: loi.de.Beer                         1                             
                   M2: transferts                          6                             
                                                                     codetransrad        1
                   M1:                                               extin               .50000
                   M2:                                               ktrou                1.00000
                   M2:                                               forme                2
                   M2:                                               rapforme             4.0
                   M2:                                               adfol               1.0
                   M2:                                               dfolbas             5.0
                   M2:                                               dfolhaut            5.0
F: croissance en biomasse                                                                
         O: seuils de températures                         2                             
                   M1: idem.LAI                            0                             
                   M2: différents.LAI                      2                             
                                                                     codtefcroi          1
                   M2:                                               temin                  .00000
                   M2:                                               temax                40.00000
         NO:                                                         teopt                12.00000
         NO:                                                         teoptbis             17.00000
         NO:                                                         efcroijuv             2.2
         NO:                                                         efcroiveg             4.250
         NO:                                                         efcroirepro           4.25000
         NO:                                                         remobres              0.2
         NO:                                                         coefmshaut             .00000
F: repartition entre organes
         NO:                                                         slamax              350
         NO:                                                         slamin              180
         NO:                                                         tigefeuil           0.5
         NO:                                                         envfruit            0.10
         NO:                                                         sea                 0.0
F: croissance et rendement                                                              
         O: type  de croissance                            2                             
                   M1: déterminée                          11                            
                   M2: indéterminée                        16                             
                                                                     codeindetermin      1
                   M1:                                               nbjgrains           30
                   M1:                                               cgrain              0.0348
                   M1:                                               cgrainv0            -0.139
                   M1:                                               nbgrmin             6000
                   O: unité.IR                             2                             
                             M11: jours                    2                             
                             M12: degré.jours              1                             
                                                                     codeir              1
                             M11:                                    vitircarb           .01100
                             M11:                                    irmax               .55000
                             M12:                                    vitircarbT          .00070
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
                                                                     codetremp           1
                   M1:                                               tminremp            .00000
                   M1:                                               tmaxremp            32.0
         NO:                                                         vitpropsucre        0.0
         NO:                                                         vitprophuile        0.0
         NO:                                                         vitirazo            0.01757
F: racines                                                                               
         NO:                                                         sensanox            0.0
         NO:                                                         stoprac             sen
         NO:                                                         sensrsec            0.0
         NO:                                                         contrdamax          0.3
         O: température pilote                             2                             
                   M1: culture                             0                             
                   M2: sol.(seuil.TGMIN)                   0                             
                                                                     codetemprac         1
         O: densité racinaire                              2                             
                   M1: profil.optimal.type                 3                             
                   M2: densité.vraie                       4                             
                                                                     coderacine          2
                   M1:                                               zlabour             22.0000
                   M1:                                               zpente              100.00000
                   M1:                                               zprlim              160.00000
                   M2:                                               draclong            80.00
                   M2:                                               debsenrac           1000.00
                   M2:                                               lvfront             5e-2
                   M2:                                               longsperac          0.0055
F: gel
         NO:                                                         tletale             -25.0
         NO:                                                         tdebgel             -4.0
         O: gel plantule ou levée                         2                                           
                   M1: non                                0 
                   M2: oui                                3
                                                                     codgellev           2
                   M2:                                               nbfgellev           2
                   M2:                                               tgellev10           -4.0
                   M2:                                               tgellev90           -20.0
         O: gel feuillage phase juvénile (jusqu'à AMF)    2                                           
                   M1: non                                0 
                   M2: oui                                2
                                                                     codgeljuv           2
                   M2:                                               tgeljuv10           -10.0
                   M2:                                               tgeljuv90           -20.0
         O: gel feuillage phase adulte                    2                                           
                   M1: non                                0 
                   M2: oui                                2
                                                                     codgelveg           2
                   M2:                                               tgelveg10           -4.5
                   M2:                                               tgelveg90           -10.0
         O: gel fleurs/fruits (à partir de FLO)           2                                           
                   M1: non                                0 
                   M2: oui                                2
                                                                     codgelflo           2
                   M2:                                               tgelflo10           -4.5
                   M2:                                               tgelflo90           -6.5
F: eau                                                                                   
         NO:                                                         psisto               15.00000
         NO:                                                         psiturg               4.00000
         NO:                                                         h2ofeuilverte       0.90
         NO:                                                         h2ofeuiljaune       0.15
         NO:                                                         h2otigestruc        0.60
         NO:                                                         h2oreserve          0.70
         NO:                                                         h2ofrvert           0.40
         NO:                                                         stdrpdes            700.0
         NO:                                                         deshydbase          0.008
         NO:                                                         tempdeshyd          0.005
         O: besoins en eau                                 2                             
                   M1: coefficient.cultural                1                             
                   M2: modèle.résistif                     1                             
                                                                     codebeso            1
                   M1:                                               kmax                1.00000
                   M2:                                               rsmin               100.00000
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
         NO:                                                         Vmax2                  .05000
         NO:                                                         Kmabs2               25000.00
         NO:                                                         adil                  5.35000
         NO:                                                         bdil                   .44000
         NO:                                                         adilmax               8.50000
         NO:                                                         bdilmax                .44000
         NO:                                                         masecNmax             1.54000
         NO:                                                         INNmin                 .300
         NO:                                                         inngrain1           1.0
         NO:                                                         inngrain2           1.2
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
                                                                     codazofruit         1
F: variétal                                                                              
         TV: les différentes variétés                      6
                   M01: Arminda                            22 
                   M02: Talent                             22 
                   M03: Thesee                             22 
                   M04: Soissons                           22 
                   M05: Promentin                          22 
                   M06: Sideral                            22 
                                                                     codevar             Arminda
                   M01:                                              stlevamf            275
                   M01:                                              stamflax            375
                   M01:                                              stlevdrp            837.
                   M01:                                              pgrainmaxi          0.03970
                   M01:                                              adens               -0.6
                   M01:                                              croirac             0.12
                   M01:                                              stflodrp            0
                   M01:                                              durvieF             200
                   O:  codebfroid                          1
                             M012:                                   jvc                 55.
                   O:  codephot                            1
                             M011:                                   sensiphot           0
                   O:  codlainet                           2
                             M011:                                   stlaxsen            675.
                             M011:                                   stsenlan            110.
                   O:  codeindetermin                      4
                             M011:                                   nbgrmax             30000
                             M011:                                   stdrpmat            700
                             M012:                                   afruitpot           .00000
                             M012:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M012:                                   fixmax              6.0
                                                                     codevar             Talent
                   M02:                                              stlevamf            245
                   M02:                                              stamflax            260
                   M02:                                              stlevdrp            692
                   M02:                                              pgrainmaxi          .03880
                   M02:                                              adens               -.60000
                   M02:                                              croirac             .12000
                   M02:                                              stflodrp            0
                   M02:                                              durvieF             200
                   O:  codebfroid                          1
                             M022:                                   jvc                 50
                   O:  codephot                            1
                             M021:                                   sensiphot           0
                   O:  codlainet                           2
                             M021:                                   stlaxsen            675.
                             M021:                                   stsenlan            110.
                   O:  codeindetermin                      4
                             M021:                                   nbgrmax             30000
                             M021:                                   stdrpmat            700
                             M022:                                   afruitpot           .00000
                             M022:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M022:                                   fixmax              6.0
                                                                     codevar             Thesee
                   M03:                                              stlevamf            245
                   M03:                                              stamflax            290
                   M03:                                              stlevdrp            732
                   M03:                                              pgrainmaxi          .0521
                   M03:                                              adens               -.60000
                   M03:                                              croirac             .12000
                   M03:                                              stflodrp            0
                   M03:                                              durvieF             200
                   O:  codebfroid                          1
                             M032:                                   jvc                 38
                   O:  codephot                            1
                             M031:                                   sensiphot           0
                   O:  codlainet                           2
                             M031:                                   stlaxsen            675.
                             M031:                                   stsenlan            110.
                   O:  codeindetermin                      4
                             M031:                                   nbgrmax             30000
                             M031:                                   stdrpmat            700
                             M032:                                   afruitpot           .00000
                             M032:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M032:                                   fixmax              6.0
                                                                     codevar             Soissons
                   M04:                                              stlevamf            245
                   M04:                                              stamflax            375
                   M04:                                              stlevdrp            722
                   M04:                                              pgrainmaxi          .0407
                   M04:                                              adens               -.60000
                   M04:                                              croirac             .12000
                   M04:                                              stflodrp            0
                   M04:                                              durvieF             200
                   O:  codebfroid                          1  
                             M042:                                   jvc                 38
                   O:  codephot                            1
                             M041:                                   sensiphot           0
                   O:  codlainet                           2
                             M041:                                   stlaxsen            675.
                             M041:                                   stsenlan            110.
                   O:  codeindetermin                      4
                             M041:                                   nbgrmax             30000
                             M041:                                   stdrpmat            700
                             M042:                                   afruitpot           .00000
                             M042:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M042:                                   fixmax              6.0
                                                                     codevar             Promentin
                   M05:                                              stlevamf            245
                   M05:                                              stamflax            310
                   M05:                                              stlevdrp            762
                   M05:                                              pgrainmaxi          .0401
                   M05:                                              adens               -.60000
                   M05:                                              croirac             .12000
                   M05:                                              stflodrp            0
                   M05:                                              durvieF             200
                   O:  codebfroid                          1
                             M052:                                   jvc                 50
                   O:  codephot                            1
                             M051:                                   sensiphot           0
                   O:  codlainet                           2
                             M051:                                   stlaxsen            675.
                             M051:                                   stsenlan            110.
                   O:  codeindetermin                      4
                             M051:                                   nbgrmax             30000
                             M051:                                   stdrpmat            700
                             M052:                                   afruitpot           .00000
                             M052:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M052:                                   fixmax              6.0
                                                                     codevar             Sideral
                   M06:                                              stlevamf            245
                   M06:                                              stamflax            260
                   M06:                                              stlevdrp            692
                   M06:                                              pgrainmaxi          .04140
                   M06:                                              adens               -.60000
                   M06:                                              croirac             .12000
                   M06:                                              stflodrp            0
                   M06:                                              durvieF             200
                   O:  codebfroid                          1
                             M062:                                   jvc                 38
                   O:  codephot                            1
                             M061:                                   sensiphot           0
                   O:  codlainet                           2
                             M061:                                   stlaxsen            675.
                             M061:                                   stsenlan            110.
                   O:  codeindetermin                      4
                             M061:                                   nbgrmax             30000
                             M061:                                   stdrpmat            700
                             M062:                                   afruitpot           .00000
                             M062:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M062:                                   fixmax              6.0
