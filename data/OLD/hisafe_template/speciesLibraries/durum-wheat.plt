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
         O: besoins en froid                               3
                   M1: non                                 0 
                   M2: vernalisation(herbacées)            4 
                   M3: dormance(ligneux)                   9
                                                                     codebfroid          2
                   M2:                                               jvcmini             7.0
                   M2:                                               julvernal           274
                   M2:                                               tfroid              6.5
                   M2:                                               ampfroid            10.0
                   O: calcul.dormance                      3
                             M31: forçage                  1
                             M32: Richardson               0
                             M33: Bidabe                   2 
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
                            M11:                                     stpltger            80.00000
                   O: croissance.plantule                  2                             
                            M21: croissance.hypocotyle     5                             
                            M22: plantation                2                             
                                                                     codehypo            1
                            M21:                                     belong              0.006
                            M21:                                     celong              2.000
                            M21:                                     elmax               4.43
                            M21:                                     nlevlim1            10
                            M21:                                     nlevlim2            50
                            M22:                                     laiplantule         0.000
                            M22:                                     nbfeuilplant        0
F: feuillage                                                                                   
         NO:                                                         plastochrone        120.0
         NO:                                                         bdens               7.000
         NO:                                                         laicomp             0.304
         NO:                                                         hautbase            0.0
         NO:                                                         hautmax             1.00
         O: fonction foliaire                              2
                   M1: LAI                                 20 
                   M2: taux.de.recouvrement                4
                                                                     codelaitr           1
                   M1:                                               vlaimax             2.2
                   M1:                                               pentlaimax          5.0
                   M1:                                               udlaimax            3.0
                   M1:                                               durvieI             0.8
                   M1:                                               tcmin               0
                   M1:                                               tcmax               40.00
                   M1:                                               ratiosen            0.8
                   M1:                                               abscission          0.0
                   M1:                                               parazofmorte        13.0
                   M1:                                               innturgmin          0.3
                   O: option.calcul.LAI                    2
                             M11: LAInet.direct            2 
                             M12: LAInet=LAIbrut-senes     4
                                                                     codlainet           2
                             M11:                                    dlaimax             .00032
                             M11:                                    tustressmin         0.7
                             M12:                                    dlaimaxbrut         3.2e-4
                             M12:                                    durviesupmax        0.4
                             M12:                                    innsen              0.35
                             M12:                                    rapsenturg          0.5
                   M2:                                               tauxrecouvmax       1.00
                   M2:                                               tauxrecouvkmax      1.0
                   M2:                                               pentrecouv          4.5
                   M2:                                               infrecouv           0.85
F: interception du rayonnement                                                           
         O: interception du rayonnement                    2                             
                   M1: loi.de.Beer                         1                             
                   M2: transferts                          6                             
                                                                     codetransrad        1
                   M1:                                               extin               0.50
                   M2:                                               ktrou               0.6
                   M2:                                               forme               1
                   M2:                                               rapforme            3.5
                   M2:                                               adfol               1.5
                   M2:                                               dfolbas             1.0
                   M2:                                               dfolhaut            2.0
F: croissance en biomasse                                                                
         O: seuils de températures                         2                             
                   M1: idem.LAI                            0                             
                   M2: différents.LAI                      2                             
                                                                     codtefcroi          1
                   M2:                                               temin                  .000
                   M2:                                               temax                40.000
         NO:                                                         teopt               10.0
         NO:                                                         teoptbis            20.0
         NO:                                                         efcroijuv             2.2
         NO:                                                         efcroiveg             4.25
         NO:                                                         efcroirepro           4.250
         NO:                                                         remobres              0.2
         NO:                                                         coefmshaut             .000
F: repartition entre organes                                                                    
         NO:                                                         slamax              300
         NO:                                                         slamin              180
         NO:                                                         tigefeuil           0.5
         NO:                                                         envfruit            0.30
         NO:                                                         sea                 80.0
F: croissance et rendement                                                              
         O: type  de croissance                            2                             
                   M1: déterminée                          11                            
                   M2: indéterminée                        16                             
                                                                     codeindetermin      1
                   M1:                                               nbjgrains           30
                   M1:                                               cgrain              0.035
                   M1:                                               cgrainv0            0.0
                   M1:                                               nbgrmin             6000
                   O: unité.IR                             2                             
                             M11: jours                    2                             
                             M12: degré.jours              1                             
                                                                     codeir              1
                             M11:                                    vitircarb           .0121
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
                   O: nombre.inflorescences                2
                             M21: imposé                   1 
                             M22: fonction.état.trophique  2
                                                                     codcalinflo         2
                             M21:                                    nbinflo             2.0
                             M22:                                    inflomax            5.0
                             M22:                                    pentinflores        0.8
         O: contrainte thermique remplissage               2                             
                   M1: oui                                 2                             
                   M2: non                                 0                             
                                                                     codetremp           1
                   M1:                                               tminremp            .00000
                   M1:                                               tmaxremp            38.0
         NO:                                                         vitpropsucre        0.0008
         NO:                                                         vitprophuile        0.0000
         NO:                                                         vitirazo            0.01757
F: racines                                                                               
         NO:                                                         sensanox            1.0
         NO:                                                         stoprac             sen
         NO:                                                         sensrsec            0.47
         NO:                                                         contrdamax          0.3
         O: température pilote                             2                             
                   M1: culture                             0                             
                   M2: sol.(seuil.TGMIN)                   0                             
                                                                     codetemprac         2
         O: densité racinaire                              2                             
                   M1: profil.optimal.type                 3                             
                   M2: densité.vraie                       4                             
                                                                     coderacine          2
                   M1:                                               zlabour             30.0000
                   M1:                                               zpente              100.00000
                   M1:                                               zprlim              200.00000
                   M2:                                               draclong            80.00
                   M2:                                               debsenrac           1000.00
                   M2:                                               lvfront             5e-2
                   M2:                                               longsperac          18182
F: gel                                                                                        
         NO:                                                         tletale             -16.0
         NO:                                                         tdebgel             -4.0
         O: gel plantule ou levée                          2                                           
                   M1: non                                 0 
                   M2: oui                                 3
                                                                     codgellev           2
                   M2:                                               nbfgellev           2
                   M2:                                               tgellev10           -4.0
                   M2:                                               tgellev90           -16.0
         O: gel feuillage phase juvénile (jusqu'à AMF)     2                                           
                   M1: non                                 0 
                   M2: oui                                 2
                                                                     codgeljuv           2
                   M2:                                               tgeljuv10           -10.0
                   M2:                                               tgeljuv90           -16.0
         O: gel feuillage phase adulte                     2                                           
                   M1: non                                 0 
                   M2: oui                                 2
                                                                     codgelveg           2
                   M2:                                               tgelveg10           -4.5
                   M2:                                               tgelveg90           -10.0
         O: gel fleurs/fruits (à partir de FLO)            2                                           
                   M1: non                                 0 
                   M2: oui                                 2
                                                                     codgelflo           2
                   M2:                                               tgelflo10           -4.5
                   M2:                                               tgelflo90           -6.5
F: eau                                                                                   
         NO:                                                         psisto              20.0
         NO:                                                         psiturg              4.0
         NO:                                                         h2ofeuilverte       0.90
         NO:                                                         h2ofeuiljaune       0.15
         NO:                                                         h2otigestruc        0.60
         NO:                                                         h2oreserve          0.70
         NO:                                                         h2ofrvert           0.40
         NO:                                                         stdrpdes            551
         NO:                                                         deshydbase          0.015
         NO:                                                         tempdeshyd          0.010
         O: besoins en eau                                 2                             
                   M1: coefficient.cultural                1                             
                   M2: modèle.résistif                     1                             
                                                                     codebeso            1
                   M1:                                               kmax                1.20000
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
         NO:                                                         Vmax2                  .05
         NO:                                                         Kmabs2               25000.00
         NO:                                                         adil                  5.3500
         NO:                                                         bdil                   .44200
         NO:                                                         adilmax               7.47000
         NO:                                                         bdilmax                .64000
         NO:                                                         masecNmax             1.54000
         NO:                                                         INNmin                 .300
         NO:                                                         inngrain1             0.90
         NO:                                                         inngrain2             1.20
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
         TV: les différentes variétés                      10
                   M01: Ambral                             22 
                   M02: allur                              22 
                   M03: Or.Jaune                           22 
                   M04: Acalou                             22 
                   M05: Ardente                            22 
                   M06: Lloyd                              22 
                   M07: Neodur                             22 
                   M08: Artimon                            22 
                   M09: Nefer                              22 
                   M10: Arcalis                            22 
                                                                     codevar             Ambral
                   M01:                                              stlevamf            245.
                   M01:                                              stamflax            290.
                   M01:                                              stlevdrp            657.
                   M01:                                              pgrainmaxi          0.042
                   M01:                                              adens               -0.6
                   M01:                                              croirac             0.12
                   M01:                                              stflodrp            10.
                   M01:                                              durvieF             200.
                   O:  codebfroid                          1
                             M012:                                   jvc                 10.
                   O:  codephot                            1
                             M011:                                   sensiphot           0.
                   O:  codlainet                           2
                             M011:                                   stlaxsen            575.
                             M011:                                   stsenlan            412.
                   O:  codeindetermin                      4
                             M011:                                   nbgrmax             19000.
                             M011:                                   stdrpmat            750.
                             M012:                                   afruitpot           .00000
                             M012:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M012:                                   fixmax              6.0
                                                                     codevar             Allur
                   M02:                                              stlevamf            695
                   M02:                                              stamflax            455
                   M02:                                              stlevdrp            1388
                   M02:                                              pgrainmaxi          .04003
                   M02:                                              adens               -0.4864
                   M02:                                              croirac             .12
                   M02:                                              stflodrp            10.
                   M02:                                              durvieF             182.
                   O:  codebfroid                          1
                             M022:                                   jvc                 26
                   O:  codephot                            1
                             M021:                                   sensiphot           0.9
                   O:  codlainet                           2
                             M021:                                   stlaxsen            960.
                             M021:                                   stsenlan            250.
                   O:  codeindetermin                      4
                             M021:                                   nbgrmax             28077
                             M021:                                   stdrpmat            522
                             M022:                                   afruitpot           .00000
                             M022:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M022:                                   fixmax              6.0
                                                                     codevar             Orjaune
                   M03:                                              stlevamf            789
                   M03:                                              stamflax            411
                   M03:                                              stlevdrp            1622
                   M03:                                              pgrainmaxi          .04850
                   M03:                                              adens               -0.5209
                   M03:                                              croirac             .20
                   M03:                                              stflodrp            10.
                   M03:                                              durvieF             199
                   O:  codebfroid                          1
                             M032:                                   jvc                 11
                   O:  codephot                            1
                             M031:                                   sensiphot           1.0
                   O:  codlainet                           2
                             M031:                                   stlaxsen            575
                             M031:                                   stsenlan            362
                   O:  codeindetermin                      4
                             M031:                                   nbgrmax             26625
                             M031:                                   stdrpmat            584
                             M032:                                   afruitpot           .00000
                             M032:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M032:                                   fixmax              6.0
                                                                     codevar             Acalou
                   M04:                                              stlevamf            228
                   M04:                                              stamflax            207
                   M04:                                              stlevdrp            695
                   M04:                                              pgrainmaxi          .05240
                   M04:                                              adens               -0.5406
                   M04:                                              croirac             .20
                   M04:                                              stflodrp            10
                   M04:                                              durvieF             170
                   O:  codebfroid                          1  
                             M042:                                   jvc                 6
                   O:  codephot                            1
                             M041:                                   sensiphot           0
                   O:  codlainet                           2
                             M041:                                   stlaxsen            703
                             M041:                                   stsenlan            362
                   O:  codeindetermin                      4
                             M041:                                   nbgrmax             20350
                             M041:                                   stdrpmat            527
                             M042:                                   afruitpot           .00000
                             M042:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M042:                                   fixmax              6.0
                                                                     codevar             Ardente
                   M05:                                              stlevamf            210
                   M05:                                              stamflax            260
                   M05:                                              stlevdrp            807
                   M05:                                              pgrainmaxi          .052
                   M05:                                              adens               -.50000
                   M05:                                              croirac             .20
                   M05:                                              stflodrp            10
                   M05:                                              durvieF             200
                   O:  codebfroid                          1
                             M052:                                   jvc                 0
                   O:  codephot                            1
                             M051:                                   sensiphot           0
                   O:  codlainet                           2
                             M051:                                   stlaxsen            575
                             M051:                                   stsenlan            412
                   O:  codeindetermin                      4
                             M051:                                   nbgrmax             20000
                             M051:                                   stdrpmat            750
                             M052:                                   afruitpot           .00000
                             M052:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M052:                                   fixmax              6.0
                                                                     codevar             Lloyd
                   M06:                                              stlevamf            755
                   M06:                                              stamflax            474
                   M06:                                              stlevdrp            1508
                   M06:                                              pgrainmaxi          .04725
                   M06:                                              adens               -0.5340
                   M06:                                              croirac             .20
                   M06:                                              stflodrp            10
                   M06:                                              durvieF             208
                   O:  codebfroid                          1
                             M062:                                   jvc                 11
                   O:  codephot                            1
                             M061:                                   sensiphot           0.9
                   O:  codlainet                           2
                             M061:                                   stlaxsen            575
                             M061:                                   stsenlan            362
                   O:  codeindetermin                      4
                             M061:                                   nbgrmax             30364
                             M061:                                   stdrpmat            562
                             M062:                                   afruitpot           .00000
                             M062:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M062:                                   fixmax              6.0
                                                                     codevar             Neodur
                   M07:                                              stlevamf            245
                   M07:                                              stamflax            290
                   M07:                                              stlevdrp            722
                   M07:                                              pgrainmaxi          .056
                   M07:                                              adens               -.5
                   M07:                                              croirac             .20
                   M07:                                              stflodrp            10
                   M07:                                              durvieF             200
                   O:  codebfroid                          1
                             M072:                                   jvc                 0
                   O:  codephot                            1
                             M071:                                   sensiphot           0
                   O:  codlainet                           2
                             M071:                                   stlaxsen            575
                             M071:                                   stsenlan            412
                   O:  codeindetermin                      4
                             M071:                                   nbgrmax             17500
                             M071:                                   stdrpmat            750
                             M072:                                   afruitpot           .00000
                             M072:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M072:                                   fixmax              6.0
                                                                     codevar             Artimon
                   M08:                                              stlevamf            390
                   M08:                                              stamflax            304
                   M08:                                              stlevdrp            901
                   M08:                                              pgrainmaxi          .03759
                   M08:                                              adens               -0.5434
                   M08:                                              croirac             .20
                   M08:                                              stflodrp            10
                   M08:                                              durvieF             213
                   O:  codebfroid                          1
                             M082:                                   jvc                 21
                   O:  codephot                            1
                             M081:                                   sensiphot           0.3
                   O:  codlainet                           2
                             M081:                                   stlaxsen            575
                             M081:                                   stsenlan            362
                   O:  codeindetermin                      4
                             M081:                                   nbgrmax             30072
                             M081:                                   stdrpmat            545
                             M082:                                   afruitpot           .00000
                             M082:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M082:                                   fixmax              6.0
                                                                     codevar             Nefer
                   M09:                                              stlevamf            363
                   M09:                                              stamflax            217
                   M09:                                              stlevdrp            858
                   M09:                                              pgrainmaxi          .04331
                   M09:                                              adens               -0.4385
                   M09:                                              croirac             .20
                   M09:                                              stflodrp            10
                   M09:                                              durvieF             169
                   O:  codebfroid                          1
                             M092:                                   jvc                 6
                   O:  codephot                            1
                             M091:                                   sensiphot           0.1
                   O:  codlainet                           2
                             M091:                                   stlaxsen            575
                             M091:                                   stsenlan            362
                   O:  codeindetermin                      4
                             M091:                                   nbgrmax             35176
                             M091:                                   stdrpmat            525
                             M092:                                   afruitpot           .00000
                             M092:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M092:                                   fixmax              6.0
                                                                     codevar             Arcalis
                   M10:                                              stlevamf            745
                   M10:                                              stamflax            399
                   M10:                                              stlevdrp            1540
                   M10:                                              pgrainmaxi          .04775
                   M10:                                              adens               -0.5365
                   M10:                                              croirac             .20
                   M10:                                              stflodrp            10
                   M10:                                              durvieF             188
                   O:  codebfroid                          1
                             M102:                                   jvc                 16
                   O:  codephot                            1
                             M101:                                   sensiphot           0.9
                   O:  codlainet                           2
                             M101:                                   stlaxsen            575
                             M101:                                   stsenlan            250
                   O:  codeindetermin                      4
                             M101:                                   nbgrmax             21662
                             M101:                                   stdrpmat            594
                             M102:                                   afruitpot           .00000
                             M102:                                   dureefruit          .00000
                   O:  codelegume                          1
                             M102:                                   fixmax              6.0
