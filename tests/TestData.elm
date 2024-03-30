module TestData exposing (..)

import Json.Encode as Enc
import Model.Post exposing (Post)
import Time


encodePost : Post -> String
encodePost post =
    let
        fields =
            [ ( "by", Enc.string post.by )
            , ( "id", Enc.int post.id )
            , ( "score", Enc.int post.score )
            , ( "time", Enc.int (Time.posixToMillis post.time // 1000) )
            , ( "title", Enc.string post.title )
            , ( "type", Enc.string post.type_ )
            ]
                ++ (post.url |> Maybe.map (\url -> [ ( "url", Enc.string url ) ]) |> Maybe.withDefault [])
    in
    fields
        |> Enc.object
        |> Enc.encode 2


posts : List Post
posts =
    [ { by = "vyrotek"
      , id = 38144400
      , score = 512
      , time = Time.millisToPosix (1699126731 * 1000)
      , title = "NewPipe – Lightweight YouTube experience for Android"
      , type_ = "story"
      , url = Just "https://newpipe.net/"
      }
    , { by = "sillysaurusx"
      , id = 38149093
      , score = 75
      , time = Time.millisToPosix (1699169400 * 1000)
      , title = "LLM companies to access largest Chinese non-fiction book collection"
      , type_ = "story"
      , url = Just "https://annas-blog.org/duxiu-exclusive.html"
      }
    , { by = "scrlk"
      , id = 38114943
      , score = 182
      , time = Time.millisToPosix (1698939114 * 1000)
      , title = "Intel Itanium IA-64 Support Removed with the Linux 6.7 Kernel"
      , type_ = "story"
      , url = Just "https://www.phoronix.com/news/Intel-IA-64-Removed-Linux-6.7"
      }
    , { by = "luu"
      , id = 38144772
      , score = 37
      , time = Time.millisToPosix (1699129495 * 1000)
      , title = "Representations / strategies for noncooperative games with imperfect information [pdf]"
      , type_ = "story"
      , url = Just "https://www2.cs.sfu.ca/~bbart/personal/masters-thesis.pdf"
      }
    , { by = "kizunajp"
      , id = 38146856
      , score = 57
      , time = Time.millisToPosix (1699145556 * 1000)
      , title = "The First App Store"
      , type_ = "story"
      , url = Just "https://one-from-nippon.ghost.io/worlds-first-app-store/"
      }
    , { by = "dlock17"
      , id = 38146154
      , score = 83
      , time = Time.millisToPosix (1699139194 * 1000)
      , title = "Show HN: Gogosseract, a Go Lib for CGo-Free Tesseract OCR via Wazero"
      , type_ = "story"
      , url = Just "https://github.com/Danlock/gogosseract"
      }
    , { by = "sohkamyung"
      , id = 38146382
      , score = 51
      , time = Time.millisToPosix (1699140957 * 1000)
      , title = "Planes, Spheres and Pseudospheres"
      , type_ = "story"
      , url = Just "https://www.gregegan.net/SCIENCE/PSP/PSP.html"
      }
    , { by = "Tepix"
      , id = 38141366
      , score = 557
      , time = Time.millisToPosix (1699108556 * 1000)
      , title = "Othello Is Solved?"
      , type_ = "story"
      , url = Just "https://arxiv.org/abs/2310.19387"
      }
    , { by = "pranay01"
      , id = 38148992
      , score = 1
      , time = Time.millisToPosix (1699167669 * 1000)
      , title = "SigNoz (YC W21), open source observability platform, hiring SREs (Remote)"
      , type_ = "job"
      , url = Just "https://signoz.io/careers/site-reliability-engineer/"
      }
    , { by = "willswire"
      , id = 38147438
      , score = 26
      , time = Time.millisToPosix (1699151020 * 1000)
      , title = "Shadows Edge Software"
      , type_ = "story"
      , url = Just "https://www.shadowsedge.mil/What-We-Do/"
      }
    , { by = "Brajeshwar"
      , id = 38115128
      , score = 58
      , time = Time.millisToPosix (1698939817 * 1000)
      , title = "Quadratic Reciprocity: The connection that changed number theory"
      , type_ = "story"
      , url = Just "https://www.quantamagazine.org/the-hidden-connection-that-changed-number-theory-20231101/"
      }
    , { by = "chriskrycho"
      , id = 38145194
      , score = 65
      , time = Time.millisToPosix (1699132523 * 1000)
      , title = "How to Do a TypeScript Conversion: an opinionated take on gradual conversions"
      , type_ = "story"
      , url = Just "https://v5.chriskrycho.com/journal/how-to-do-a-typescript-conversion/"
      }
    , { by = "scyzoryk_xyz"
      , id = 38144266
      , score = 83
      , time = Time.millisToPosix (1699125874 * 1000)
      , title = "FTC Chair Lina Khan looks for allies and leads in Silicon Valley charm offensive"
      , type_ = "story"
      , url = Just "https://www.reuters.com/business/ftc-chair-lina-khan-looks-allies-leads-silicon-valley-charm-offensive-2023-11-03/"
      }
    , { by = "badrabbit"
      , id = 38147459
      , score = 96
      , time = Time.millisToPosix (1699151182 * 1000)
      , title = "Topic: Discord Stealer"
      , type_ = "story"
      , url = Just "https://github.com/topics/discord-stealer"
      }
    , { by = "michelangelo"
      , id = 38146122
      , score = 132
      , time = Time.millisToPosix (1699138933 * 1000)
      , title = "SARS-CoV-2 Infection Affects Energy Stores in the Body, Causing Organ Failure"
      , type_ = "story"
      , url = Just "https://news.unchealthcare.org/2023/10/researchers-show-sars-cov-2-infection-affects-energy-stores-in-the-body-causing-organ-failure/"
      }
    , { by = "RadixDLT"
      , id = 38146696
      , score = 102
      , time = Time.millisToPosix (1699143985 * 1000)
      , title = "Soda additive linked to thyroid toxicity may finally get banned by FDA"
      , type_ = "story"
      , url = Just "https://arstechnica.com/health/2023/11/50-years-later-fda-proposes-banning-a-food-additive-it-said-is-not-safe/"
      }
    , { by = "bauta-steen"
      , id = 38147052
      , score = 56
      , time = Time.millisToPosix (1699147453 * 1000)
      , title = "BeeWare Toga v0.4.0 – A Python native, OS native GUI toolkit"
      , type_ = "story"
      , url = Just "https://github.com/beeware/toga/releases/tag/v0.4.0"
      }
    , { by = "selimthegrim"
      , id = 38145054
      , score = 105
      , time = Time.millisToPosix (1699131495 * 1000)
      , title = "The cottage industry of YouTube obituary pirates"
      , type_ = "story"
      , url = Just "https://www.wired.com/story/youtube-obituary-pirates/"
      }
    , { by = "PaulHoule"
      , id = 38115221
      , score = 44
      , time = Time.millisToPosix (1698940132 * 1000)
      , title = "Chemists, engineers craft adjustable arrays of microscopic lenses"
      , type_ = "story"
      , url = Just "https://phys.org/news/2023-10-chemists-craft-adjustable-arrays-microscopic.html"
      }
    , { by = "bricemo"
      , id = 38146327
      , score = 62
      , time = Time.millisToPosix (1699140572 * 1000)
      , title = "Disturbed Lands (National Park Service)"
      , type_ = "story"
      , url = Just "https://www.nps.gov/shen/learn/nature/disturbedlands.htm"
      }
    , { by = "benbreen"
      , id = 38147981
      , score = 84
      , time = Time.millisToPosix (1699156421 * 1000)
      , title = "The world nearly adopted a calendar with 13 months of 28 days"
      , type_ = "story"
      , url = Just "https://www.washingtonpost.com/history/2023/11/04/battle-sabbath-13-month-calendar/"
      }
    , { by = "belter"
      , id = 38143984
      , score = 106
      , time = Time.millisToPosix (1699123807 * 1000)
      , title = "AI and Open Source in 2023"
      , type_ = "story"
      , url = Just "https://magazine.sebastianraschka.com/p/ai-and-open-source-in-2023"
      }
    , { by = "elvis70"
      , id = 38132016
      , score = 1114
      , time = Time.millisToPosix (1699032376 * 1000)
      , title = "As I retire, my goal now is to release 40+ years of source code"
      , type_ = "story"
      , url = Just "https://dunfield.themindfactory.com/dnldsrc.htm"
      }
    , { by = "croes"
      , id = 38145461
      , score = 122
      , time = Time.millisToPosix (1699134199 * 1000)
      , title = "New Microsoft Exchange zero-days allow RCE, data theft attacks"
      , type_ = "story"
      , url = Just "https://www.bleepingcomputer.com/news/microsoft/new-microsoft-exchange-zero-days-allow-rce-data-theft-attacks/"
      }
    , { by = "zone411"
      , id = 38148396
      , score = 125
      , time = Time.millisToPosix (1699160553 * 1000)
      , title = "Grok"
      , type_ = "story"
      , url = Just "https://twitter.com/xai/status/1721027348970238035"
      }
    , { by = "edent"
      , id = 38139687
      , score = 220
      , time = Time.millisToPosix (1699093638 * 1000)
      , title = "A 6 channel GPS receiver from 1993"
      , type_ = "story"
      , url = Just "https://mastodon.sdf.org/@keelan/111349948124943603"
      }
    , { by = "stevefan1999"
      , id = 38137373
      , score = 313
      , time = Time.millisToPosix (1699063305 * 1000)
      , title = "AMD MicroBlaze V Processor: A Flexible and Efficient RISC-V Processor"
      , type_ = "story"
      , url = Just "https://www.xilinx.com/products/design-tools/microblaze-v.html"
      }
    , { by = "thunderbong"
      , id = 38114871
      , score = 112
      , time = Time.millisToPosix (1698938877 * 1000)
      , title = "Lahaina Noon"
      , type_ = "story"
      , url = Just "https://en.wikipedia.org/wiki/Lahaina_Noon"
      }
    , { by = "acifani"
      , id = 38140242
      , score = 157
      , time = Time.millisToPosix (1699099310 * 1000)
      , title = "Spin 2.0 – open-source tool for building and running WASM apps"
      , type_ = "story"
      , url = Just "https://www.fermyon.com/blog/introducing-spin-v2"
      }
    , { by = "mNovak"
      , id = 38120388
      , score = 411
      , time = Time.millisToPosix (1698960362 * 1000)
      , title = "Tell HN: Submit comments to IRS re tax treatment of software dev expenses"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "the_v01d"
      , id = 38145368
      , score = 11
      , time = Time.millisToPosix (1699133640 * 1000)
      , title = "Show HN: A tiny and platform-agnostic true random number generator for FPGA/ASIC"
      , type_ = "story"
      , url = Just "https://github.com/stnolting/neoTRNG"
      }
    , { by = "belltaco"
      , id = 38146837
      , score = 99
      , time = Time.millisToPosix (1699145268 * 1000)
      , title = "Daylight saving time can seriously affect your health"
      , type_ = "story"
      , url = Just "https://apnews.com/article/daylight-saving-2023-fall-back-a3738d5c74301b8068825927c49fc6b4"
      }
    , { by = "asimpleusecase"
      , id = 38146211
      , score = 46
      , time = Time.millisToPosix (1699139694 * 1000)
      , title = "Delhi air pollution spikes to 100 times WHO health limit"
      , type_ = "story"
      , url = Just "https://www.theguardian.com/world/2023/nov/03/delhi-india-air-quality-pollution-spike-world-health-organization-limit"
      }
    , { by = "robomartin"
      , id = 38148119
      , score = 5
      , time = Time.millisToPosix (1699157746 * 1000)
      , title = "A game. The only winning move is not to play"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "roody15"
      , id = 38147606
      , score = 83
      , time = Time.millisToPosix (1699152889 * 1000)
      , title = "Max is taking 4K away from its legacy ad-free subscribers"
      , type_ = "story"
      , url = Just "https://www.theverge.com/2023/11/2/23943859/max-4k-hbo-max-ad-free-subscribers"
      }
    , { by = "GOPbIHbI4"
      , id = 38137879
      , score = 132
      , time = Time.millisToPosix (1699068531 * 1000)
      , title = "Classes vs. structs in .NET: how not to teach about performance"
      , type_ = "story"
      , url = Just "https://sergeyteplyakov.github.io/Blog/benchmarking/2023/11/02/Performance_Comparison_For_Classes_vs_Structs.html"
      }
    , { by = "geox"
      , id = 38115323
      , score = 68
      , time = Time.millisToPosix (1698940455 * 1000)
      , title = "How “blue” and “green” appeared in a language that didn’t have words for them"
      , type_ = "story"
      , url = Just "https://news.mit.edu/2023/how-blue-and-green-appeared-language-1102"
      }
    , { by = "chen_dev"
      , id = 38146738
      , score = 17
      , time = Time.millisToPosix (1699144434 * 1000)
      , title = "Oracle Cloud Outage"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "neustradamus"
      , id = 38146711
      , score = 15
      , time = Time.millisToPosix (1699144150 * 1000)
      , title = "Exim 4.97.0 – SMTP Mail Server – Message Transfer Agent (MTA)"
      , type_ = "story"
      , url = Just "https://www.exim.org/"
      }
    , { by = "meandave"
      , id = 38144530
      , score = 82
      , time = Time.millisToPosix (1699127713 * 1000)
      , title = "You're Gonna Need a Bigger Browser"
      , type_ = "story"
      , url = Just "https://berjon.com/bigger-browser/"
      }
    , { by = "kvnhn"
      , id = 38138085
      , score = 157
      , time = Time.millisToPosix (1699071397 * 1000)
      , title = "Making \"LCD, Please\""
      , type_ = "story"
      , url = Just "https://dukope.com/devlogs/papers-please/lcdplease/"
      }
    , { by = "vintagedave"
      , id = 38145097
      , score = 17
      , time = Time.millisToPosix (1699131874 * 1000)
      , title = "Interview with a Gladiatrix"
      , type_ = "story"
      , url = Just "https://antigonejournal.com/2023/10/modern-roman-gladiatrix/"
      }
    , { by = "dgudkov"
      , id = 38146809
      , score = 6
      , time = Time.millisToPosix (1699145056 * 1000)
      , title = "Towards Modern Development of Cloud Applications [pdf]"
      , type_ = "story"
      , url = Just "https://sigops.org/s/conferences/hotos/2023/papers/ghemawat.pdf"
      }
    , { by = "lxm"
      , id = 38137377
      , score = 224
      , time = Time.millisToPosix (1699063376 * 1000)
      , title = "A blog post is a long and complex search query to find people (2022)"
      , type_ = "story"
      , url = Just "https://www.henrikkarlsson.xyz/p/search-query"
      }
    , { by = "danboarder"
      , id = 38143369
      , score = 68
      , time = Time.millisToPosix (1699119933 * 1000)
      , title = "After decades of dreams, a commercial spaceplane is almost ready to fly"
      , type_ = "story"
      , url = Just "https://arstechnica.com/space/2023/11/after-decades-of-dreams-a-commercial-spaceplane-is-almost-ready-to-fly/"
      }
    , { by = "rom1v"
      , id = 38143818
      , score = 110
      , time = Time.millisToPosix (1699122631 * 1000)
      , title = "Scrcpy 2.2 adds Android camera mirroring"
      , type_ = "story"
      , url = Just "https://github.com/Genymobile/scrcpy/releases/tag/v2.2"
      }
    , { by = "kristianp"
      , id = 38148171
      , score = 52
      , time = Time.millisToPosix (1699158216 * 1000)
      , title = "SpaceX poised for 'mid-November' launch of second Starship test flight"
      , type_ = "story"
      , url = Just "https://spaceflightnow.com/2023/11/04/spacex-poised-for-mid-november-of-second-starship-test-launch/"
      }
    , { by = "iamflimflam1"
      , id = 38142606
      , score = 160
      , time = Time.millisToPosix (1699115786 * 1000)
      , title = "I need more USB power"
      , type_ = "story"
      , url = Just "https://atomic14.substack.com/p/i-need-more-usb-power"
      }
    , { by = "guiambros"
      , id = 38147815
      , score = 8
      , time = Time.millisToPosix (1699155085 * 1000)
      , title = "Ancient coins have been found off Sardinia, probably from shipwreck"
      , type_ = "story"
      , url = Just "https://apnews.com/article/italy-ancient-coins-sardinia-divers-cabdfa070d37a8020f874dd90c0a2433"
      }
    , { by = "georgehill"
      , id = 38144124
      , score = 65
      , time = Time.millisToPosix (1699124835 * 1000)
      , title = "C++ Sudoku Solver by Singapore PM Lee Hsien Loong"
      , type_ = "story"
      , url = Just "https://twitter.com/adad8m/status/1720822885496049863"
      }
    , { by = "samclemens"
      , id = 38144584
      , score = 91
      , time = Time.millisToPosix (1699128056 * 1000)
      , title = "Lapham's Quarterly Is on Hiatus"
      , type_ = "story"
      , url = Just "https://www.laphamsquarterly.org/roundtable/hiatus"
      }
    , { by = "oferzelig"
      , id = 38148691
      , score = 6
      , time = Time.millisToPosix (1699163845 * 1000)
      , title = "X.ai Grok"
      , type_ = "story"
      , url = Just "https://grok.x.ai/"
      }
    , { by = "alexzeitler"
      , id = 38134861
      , score = 219
      , time = Time.millisToPosix (1699044810 * 1000)
      , title = "Asciinema: Record and share your terminal sessions, the simple way"
      , type_ = "story"
      , url = Just "https://asciinema.org/"
      }
    , { by = "perihelions"
      , id = 38121250
      , score = 605
      , time = Time.millisToPosix (1698964208 * 1000)
      , title = "In 1886, the US commissioned watercolor paintings of every known fruit (2019)"
      , type_ = "story"
      , url = Just "https://www.openculture.com/2019/06/the-us-government-commissioned-7500-watercolor-paintings.html"
      }
    , { by = "ShMcK"
      , id = 38128699
      , score = 467
      , time = Time.millisToPosix (1699019273 * 1000)
      , title = "No dogs were harmed in the making of this app"
      , type_ = "story"
      , url = Just "https://shmck.substack.com/p/no-dogs-were-harmed-in-the-making"
      }
    , { by = "kiyanwang"
      , id = 38125348
      , score = 417
      , time = Time.millisToPosix (1698994231 * 1000)
      , title = "Bringing garbage collected programming languages efficiently to WebAssembly"
      , type_ = "story"
      , url = Just "https://v8.dev/blog/wasm-gc-porting"
      }
    , { by = "_Microft"
      , id = 38135265
      , score = 225
      , time = Time.millisToPosix (1699047160 * 1000)
      , title = "Wooden Game Boy"
      , type_ = "story"
      , url = Just "https://there.oughta.be/a/wooden-game-boy"
      }
    , { by = "bookofjoe"
      , id = 38114382
      , score = 80
      , time = Time.millisToPosix (1698937118 * 1000)
      , title = "The Apple II Circuit Description (1983)"
      , type_ = "story"
      , url = Just "https://archive.org/details/taiicd"
      }
    , { by = "przmk"
      , id = 38144417
      , score = 304
      , time = Time.millisToPosix (1699126785 * 1000)
      , title = "Bevy 0.12"
      , type_ = "story"
      , url = Just "https://bevyengine.org/news/bevy-0-12/"
      }
    , { by = "Terretta"
      , id = 38136863
      , score = 203
      , time = Time.millisToPosix (1699058450 * 1000)
      , title = "Telling GPT-4 you're scared or under pressure improves performance"
      , type_ = "story"
      , url = Just "https://aimodels.substack.com/p/telling-gpt-4-youre-scared-or-under"
      }
    , { by = "donohoe"
      , id = 38122061
      , score = 1952
      , time = Time.millisToPosix (1698968957 * 1000)
      , title = "Sam Bankman-Fried Convicted"
      , type_ = "story"
      , url = Just "https://www.nytimes.com/live/2023/11/02/business/sam-bankman-fried-trial"
      }
    , { by = "austinallegro"
      , id = 38135810
      , score = 188
      , time = Time.millisToPosix (1699050091 * 1000)
      , title = "Surround sound test files for every audio format"
      , type_ = "story"
      , url = Just "https://drive.google.com/drive/folders/1JxmeedtAtgmoafXv9rroiDOS2vEX7N4b"
      }
    , { by = "belter"
      , id = 38146339
      , score = 128
      , time = Time.millisToPosix (1699140638 * 1000)
      , title = "Tesla strike widens as Swedish union expands blockade"
      , type_ = "story"
      , url = Just "https://www.thelocal.se/20231103/tesla-strike-widens-as-swedish-union-expands-blockade"
      }
    , { by = "Vigier"
      , id = 38137750
      , score = 15
      , time = Time.millisToPosix (1699067013 * 1000)
      , title = "Rāhui and the Art of Marine Conservation"
      , type_ = "story"
      , url = Just "https://hakaimagazine.com/features/rahui-and-the-art-of-marine-conservation/"
      }
    , { by = "diodorus"
      , id = 38137713
      , score = 81
      , time = Time.millisToPosix (1699066588 * 1000)
      , title = "Large-scale violence in Late Neolithic Western Europe based on skeletal evidence"
      , type_ = "story"
      , url = Just "https://www.nature.com/articles/s41598-023-43026-9"
      }
    , { by = "wjSgoWPm5bWAhXB"
      , id = 38127906
      , score = 224
      , time = Time.millisToPosix (1699015662 * 1000)
      , title = "First observation of a virus attaching to another virus"
      , type_ = "story"
      , url = Just "https://umbc.edu/stories/first-observed-virus-attaching-to-another/"
      }
    , { by = "leidenfrost"
      , id = 38141173
      , score = 185
      , time = Time.millisToPosix (1699107231 * 1000)
      , title = "KDE desktop cube effect returns"
      , type_ = "story"
      , url = Just "https://www.phoronix.com/news/KDE-Wayland-Color-Mgmt"
      }
    , { by = "kbns"
      , id = 38142734
      , score = 49
      , time = Time.millisToPosix (1699116526 * 1000)
      , title = "Searching for hidden silicon art on a microchip"
      , type_ = "story"
      , url = Just "https://twitter.com/microscopicture/status/1720837951238636018"
      }
    , { by = "MilnerRoute"
      , id = 38146445
      , score = 19
      , time = Time.millisToPosix (1699141527 * 1000)
      , title = "U.S. cities consider banning \"right on red\" laws amid rise in pedestrian deaths"
      , type_ = "story"
      , url = Just "https://www.cbsnews.com/news/pedestrian-deaths-right-on-red-ban/"
      }
    , { by = "elisaado"
      , id = 38141320
      , score = 195
      , time = Time.millisToPosix (1699108277 * 1000)
      , title = "Paste without formatting on macOS"
      , type_ = "story"
      , url = Just "https://scottswezey.com/always-paste-without-formatting-macos/"
      }
    , { by = "jart"
      , id = 38101613
      , score = 1187
      , time = Time.millisToPosix (1698859540 * 1000)
      , title = "Cosmopolitan Third Edition"
      , type_ = "story"
      , url = Just "https://justine.lol/cosmo3/"
      }
    , { by = "pndy"
      , id = 38110679
      , score = 126
      , time = Time.millisToPosix (1698915300 * 1000)
      , title = "Bibi-binary"
      , type_ = "story"
      , url = Just "https://en.wikipedia.org/wiki/Bibi-binary"
      }
    , { by = "intunderflow"
      , id = 38144047
      , score = 57
      , time = Time.millisToPosix (1699124257 * 1000)
      , title = "UK plans to redefine extremism to include undermining the country"
      , type_ = "story"
      , url = Just "https://www.theguardian.com/uk-news/2023/nov/04/plans-to-redefine-extremism-would-include-undermining-uk-values"
      }
    , { by = "Tomte"
      , id = 38117556
      , score = 453
      , time = Time.millisToPosix (1698947997 * 1000)
      , title = "Light can make water evaporate without heat"
      , type_ = "story"
      , url = Just "https://news.mit.edu/2023/surprising-finding-light-makes-water-evaporate-without-heat-1031"
      }
    , { by = "nullhole"
      , id = 38141328
      , score = 13
      , time = Time.millisToPosix (1699108323 * 1000)
      , title = "The Trust Principles"
      , type_ = "story"
      , url = Just "https://www.thomsonreuters.com/en/about-us/trust-principles.html"
      }
    , { by = "mooreds"
      , id = 38140217
      , score = 15
      , time = Time.millisToPosix (1699099081 * 1000)
      , title = "Coding in Public: Help Battle Imposter Syndrome and Inspire Others"
      , type_ = "story"
      , url = Just "https://www.codingtemple.com/blog/coding-in-public-help-battle-imposter-syndrome-and-inspire-others/"
      }
    , { by = "jlpcsl"
      , id = 38143180
      , score = 257
      , time = Time.millisToPosix (1699118819 * 1000)
      , title = "Software that supports your body should always respect your freedom"
      , type_ = "story"
      , url = Just "https://www.fsf.org/blogs/community/software-that-supports-your-body-should-always-respect-your-freedom"
      }
    , { by = "rblank"
      , id = 38133134
      , score = 117
      , time = Time.millisToPosix (1699037104 * 1000)
      , title = "Show HN: MicroLua – Lua for the RP2040 Microcontroller"
      , type_ = "story"
      , url = Just "https://github.com/MicroLua/MicroLua"
      }
    , { by = "eastdakota"
      , id = 38138640
      , score = 451
      , time = Time.millisToPosix (1699078841 * 1000)
      , title = "Post Mortem on Cloudflare Control Plane and Analytics Outage"
      , type_ = "story"
      , url = Just "https://blog.cloudflare.com/post-mortem-on-cloudflare-control-plane-and-analytics-outage/"
      }
    , { by = "nateb2022"
      , id = 38136607
      , score = 543
      , time = Time.millisToPosix (1699055994 * 1000)
      , title = "Portugal. The Man – Official Website Is a Google Sheets Document"
      , type_ = "story"
      , url = Just "https://www.portugaltheman.com/?frontpage=true"
      }
    , { by = "ingve"
      , id = 38125771
      , score = 212
      , time = Time.millisToPosix (1698998390 * 1000)
      , title = "M3 Macs: there's more to performance than counting cores"
      , type_ = "story"
      , url = Just "https://eclecticlight.co/2023/11/03/m3-macs-theres-more-to-performance-than-counting-cores/"
      }
    , { by = "Thevet"
      , id = 38133699
      , score = 43
      , time = Time.millisToPosix (1699038883 * 1000)
      , title = "What is a Teddy Roosevelt presidential library doing in North Dakota?"
      , type_ = "story"
      , url = Just "https://www.nytimes.com/2023/10/27/arts/theodore-roosevelt-presidential-library.html"
      }
    , { by = "vincelt"
      , id = 38141913
      , score = 5
      , time = Time.millisToPosix (1699111577 * 1000)
      , title = "You Are Not Late (2014)"
      , type_ = "story"
      , url = Just "https://kk.org/thetechnium/you-are-not-late/"
      }
    , { by = "simonebrunozzi"
      , id = 38140697
      , score = 156
      , time = Time.millisToPosix (1699103265 * 1000)
      , title = "Shinzo Abe's Assassination"
      , type_ = "story"
      , url = Just "https://www.theatlantic.com/magazine/archive/2023/10/shinzo-abe-assassination-japan-unification-church-moonies/675114/"
      }
    , { by = "goranmoomin"
      , id = 38137975
      , score = 101
      , time = Time.millisToPosix (1699069571 * 1000)
      , title = "Sometimes, it is a compiler bug (2022)"
      , type_ = "story"
      , url = Just "https://quick-lint-js.com/blog/bug-journey/"
      }
    , { by = "andyfrancis"
      , id = 38130089
      , score = 154
      , time = Time.millisToPosix (1699024665 * 1000)
      , title = "Guide to Adopting AV1 Encoding"
      , type_ = "story"
      , url = Just "https://bitmovin.com/av1/av1-encoding-guide/"
      }
    , { by = "lawgimenez"
      , id = 38146887
      , score = 7
      , time = Time.millisToPosix (1699145876 * 1000)
      , title = "Ask HN: Where do you get your health news?"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "zdw"
      , id = 38142586
      , score = 37
      , time = Time.millisToPosix (1699115620 * 1000)
      , title = "Mass producing the most expensive rice cooker [video]"
      , type_ = "story"
      , url = Just "https://www.youtube.com/watch?v=xLCwr8qG1p4"
      }
    , { by = "clouddrover"
      , id = 38135029
      , score = 32
      , time = Time.millisToPosix (1699045679 * 1000)
      , title = "Codec Royalties on Content and the Jaws Moment"
      , type_ = "story"
      , url = Just "https://streaminglearningcenter.com/codecs/codec-royalties-on-content.html"
      }
    , { by = "tintinnabula"
      , id = 38119463
      , score = 139
      , time = Time.millisToPosix (1698956195 * 1000)
      , title = "Louis Armstrong's Last Word"
      , type_ = "story"
      , url = Just "https://www.thenation.com/article/culture/louis-armstrong-archive/"
      }
    , { by = "austinallegro"
      , id = 38136041
      , score = 111
      , time = Time.millisToPosix (1699051418 * 1000)
      , title = "The Sega Hitachi HiSaturn Navi Console"
      , type_ = "story"
      , url = Just "https://nfggames.com/games/hisaturn/"
      }
    , { by = "nixass"
      , id = 38146676
      , score = 124
      , time = Time.millisToPosix (1699143820 * 1000)
      , title = "My experience taking Tesla to court about FSD"
      , type_ = "story"
      , url = Just "https://teslamotorsclub.com/tmc/threads/my-experience-taking-tesla-to-court-about-fsd.315086/"
      }
    , { by = "Tomte"
      , id = 38126623
      , score = 179
      , time = Time.millisToPosix (1699006623 * 1000)
      , title = "Pix2tex: Using a ViT to convert images of equations into LaTeX code"
      , type_ = "story"
      , url = Just "https://github.com/lukas-blecher/LaTeX-OCR"
      }
    , { by = "plurby"
      , id = 38110072
      , score = 470
      , time = Time.millisToPosix (1698910353 * 1000)
      , title = "Talk-Llama"
      , type_ = "story"
      , url = Just "https://github.com/ggerganov/whisper.cpp/tree/master/examples/talk-llama"
      }
    , { by = "pierre-renaux"
      , id = 38140833
      , score = 145
      , time = Time.millisToPosix (1699104640 * 1000)
      , title = "Cooperative C++ Evolution – Toward a TypeScript for C++"
      , type_ = "story"
      , url = Just "https://herbsutter.com/2023/10/09/my-new-cppcon-talk-is-on-youtube-cooperative-c-evolution-toward-a-typescript-for-c/"
      }
    , { by = "petabyt"
      , id = 38137119
      , score = 35
      , time = Time.millisToPosix (1699060729 * 1000)
      , title = "The StahlDream"
      , type_ = "story"
      , url = Just "https://remexre.com/stahl/dream"
      }
    , { by = "thunderbong"
      , id = 38125256
      , score = 112
      , time = Time.millisToPosix (1698993413 * 1000)
      , title = "Frink is a programming language designed to make physical calculations simple"
      , type_ = "story"
      , url = Just "https://frinklang.org/"
      }
    , { by = "ourmandave"
      , id = 38147278
      , score = 38
      , time = Time.millisToPosix (1699149573 * 1000)
      , title = "SBF Is Headed to Prison but FTX Will Haunt the Crypto Community for Years"
      , type_ = "story"
      , url = Just "https://gizmodo.com/ftx-fbi-sam-bankman-fried-cryptocurrency-data-1850990825"
      }
    , { by = "Tomte"
      , id = 38134935
      , score = 59
      , time = Time.millisToPosix (1699045201 * 1000)
      , title = "Yorick is an interpreted programming language for scientific simulations"
      , type_ = "story"
      , url = Just "https://yorick.sourceforge.net/"
      }
    , { by = "raybb"
      , id = 38141006
      , score = 149
      , time = Time.millisToPosix (1699106066 * 1000)
      , title = "Starfish bodies aren’t bodies at all, study finds"
      , type_ = "story"
      , url = Just "https://www.cnn.com/2023/11/02/world/starfish-head-body-plan-scn/index.html"
      }
    , { by = "eXpl0it3r"
      , id = 38145274
      , score = 7
      , time = Time.millisToPosix (1699133056 * 1000)
      , title = "SFML 2.6.1"
      , type_ = "story"
      , url = Just "https://github.com/SFML/SFML/releases/tag/2.6.1"
      }
    , { by = "oalders"
      , id = 38136211
      , score = 21
      , time = Time.millisToPosix (1699052539 * 1000)
      , title = "Running Mod_Perl with Mojolicious in Docker"
      , type_ = "story"
      , url = Just "https://rawley.xyz/docker-mod-perl.html"
      }
    , { by = "belter"
      , id = 38141433
      , score = 67
      , time = Time.millisToPosix (1699108947 * 1000)
      , title = "The Great Inflation"
      , type_ = "story"
      , url = Just "https://www.federalreservehistory.org/essays/great-inflation"
      }
    , { by = "apsec112"
      , id = 38122631
      , score = 174
      , time = Time.millisToPosix (1698971680 * 1000)
      , title = "MetNet-3: A state-of-the-art neural weather model"
      , type_ = "story"
      , url = Just "https://blog.research.google/2023/11/metnet-3-state-of-art-neural-weather.html?m=1"
      }
    , { by = "dangoldin"
      , id = 38118577
      , score = 264
      , time = Time.millisToPosix (1698952545 * 1000)
      , title = "From S3 to R2: An economic opportunity"
      , type_ = "story"
      , url = Just "https://dansdatathoughts.substack.com/p/from-s3-to-r2-an-economic-opportunity"
      }
    , { by = "whoishiring"
      , id = 38099086
      , score = 481
      , time = Time.millisToPosix (1698850843 * 1000)
      , title = "Ask HN: Who is hiring? (November 2023)"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "Zolomon"
      , id = 38120598
      , score = 208
      , time = Time.millisToPosix (1698961263 * 1000)
      , title = "Course using F#: Write your own tiny programming system(s)"
      , type_ = "story"
      , url = Just "https://d3s.mff.cuni.cz/teaching/nprg077/"
      }
    , { by = "belter"
      , id = 38144707
      , score = 11
      , time = Time.millisToPosix (1699128971 * 1000)
      , title = "Astra, low on cash, defaults on loan"
      , type_ = "story"
      , url = Just "https://spacenews.com/astra-low-on-cash-defaults-on-loan/"
      }
    , { by = "derbOac"
      , id = 38138187
      , score = 204
      , time = Time.millisToPosix (1699072786 * 1000)
      , title = "Maersk to cut 10k jobs as shipping demand drops"
      , type_ = "story"
      , url = Just "https://www.reuters.com/business/shipping-giant-maersk-q3-above-expectation-sees-fy-lower-end-range-2023-11-03/"
      }
    , { by = "stonepresto"
      , id = 38145205
      , score = 52
      , time = Time.millisToPosix (1699132571 * 1000)
      , title = "Welcome to the New GamersNexus Website v5.0: A Message"
      , type_ = "story"
      , url = Just "https://gamersnexus.net/gn-extras/welcome-new-gamersnexus-website-v50-message"
      }
    , { by = "ericdanielski"
      , id = 38139011
      , score = 252
      , time = Time.millisToPosix (1699084788 * 1000)
      , title = "How to stop adulterated turmeric from killing people"
      , type_ = "story"
      , url = Just "https://www.economist.com/leaders/2023/11/02/how-to-stop-turmeric-from-killing-people"
      }
    , { by = "prismatic"
      , id = 38132331
      , score = 38
      , time = Time.millisToPosix (1699033599 * 1000)
      , title = "The Miracle of Photography"
      , type_ = "story"
      , url = Just "https://themillions.com/2023/11/the-miracle-of-photography.html"
      }
    , { by = "dguo"
      , id = 38135892
      , score = 71
      , time = Time.millisToPosix (1699050542 * 1000)
      , title = "Edison Bulb Night Mode"
      , type_ = "story"
      , url = Just "https://www.petemillspaugh.com/edison-bulb"
      }
    , { by = "nabla9"
      , id = 38111449
      , score = 155
      , time = Time.millisToPosix (1698921003 * 1000)
      , title = "RegEx Crossword (2013)"
      , type_ = "story"
      , url = Just "https://jimbly.github.io/regex-crossword/"
      }
    , { by = "picture"
      , id = 38136783
      , score = 65
      , time = Time.millisToPosix (1699057669 * 1000)
      , title = "CPU-Z's Inadequate Benchmark"
      , type_ = "story"
      , url = Just "https://chipsandcheese.com/2023/11/03/cpu-zs-inadequate-benchmark/"
      }
    , { by = "11thEarlOfMar"
      , id = 38146443
      , score = 29
      , time = Time.millisToPosix (1699141489 * 1000)
      , title = "DEA Launches Crack Down on San Francisco Fentanyl Dealing"
      , type_ = "story"
      , url = Just "https://abc7news.com/sf-fentanyl-drug-dealers-all-hands-of-deck-operation-sfpd/14006658/"
      }
    , { by = "PaulHoule"
      , id = 38136689
      , score = 205
      , time = Time.millisToPosix (1699056717 * 1000)
      , title = "Uranium demand hits decade high as nuclear renaissance gains traction"
      , type_ = "story"
      , url = Just "https://oilprice.com/The-Environment/Global-Warming/Uranium-Demand-Hits-Decade-High-As-Nuclear-Renaissance-Gains-Traction.html"
      }
    , { by = "EvgeniyZh"
      , id = 38134120
      , score = 56
      , time = Time.millisToPosix (1699041189 * 1000)
      , title = "Accelerating digital block design with Google's open source Mid-Level Synthesis"
      , type_ = "story"
      , url = Just "https://antmicro.com/blog/2023/09/accelerating-digital-block-design-with-googles-xls/"
      }
    , { by = "kennyfrc"
      , id = 38143844
      , score = 26
      , time = Time.millisToPosix (1699122816 * 1000)
      , title = "Show HN: Cami.js – A No Build, Web Component Based Reactive Framework"
      , type_ = "story"
      , url = Just "https://github.com/kennyfrc/cami.js"
      }
    , { by = "abahlo"
      , id = 38126210
      , score = 177
      , time = Time.millisToPosix (1699002633 * 1000)
      , title = "Why You Should Write Your Own Static Site Generator"
      , type_ = "story"
      , url = Just "https://arne.me/articles/write-your-own-ssg"
      }
    , { by = "teleforce"
      , id = 38137867
      , score = 12
      , time = Time.millisToPosix (1699068326 * 1000)
      , title = "GiNaC: Open framework for symbolic computation within C++ programming language"
      , type_ = "story"
      , url = Just "https://www.ginac.de/tutorial/"
      }
    , { by = "keepamovin"
      , id = 38126068
      , score = 39
      , time = Time.millisToPosix (1699001233 * 1000)
      , title = "Liblinux – architecture-independent access to Linux system calls"
      , type_ = "story"
      , url = Just "https://github.com/matheusmoreira/liblinux"
      }
    , { by = "transpute"
      , id = 38125379
      , score = 111
      , time = Time.millisToPosix (1698994522 * 1000)
      , title = "Enable ARMv9 Memory Tagging Extension (MTE) on Pixel 8"
      , type_ = "story"
      , url = Just "https://outflux.net/blog/archives/2023/10/26/enable-mte-on-pixel-8/"
      }
    , { by = "benbreen"
      , id = 38134303
      , score = 56
      , time = Time.millisToPosix (1699042066 * 1000)
      , title = "Why is RC Cola popular in Tajikistan?"
      , type_ = "story"
      , url = Just "https://www.snackstack.net/p/why-is-rc-cola-popular-in-tajikistan"
      }
    , { by = "eternityforest"
      , id = 38139236
      , score = 7
      , time = Time.millisToPosix (1699088114 * 1000)
      , title = "Show HN: I started building yet another CSS framework"
      , type_ = "story"
      , url = Just "https://eternityforest.github.io/barrel.css/"
      }
    , { by = "alexmolas"
      , id = 38126063
      , score = 85
      , time = Time.millisToPosix (1699001204 * 1000)
      , title = "How to help someone use a computer (1996)"
      , type_ = "story"
      , url = Just "https://pages.gseis.ucla.edu/faculty/agre/how-to-help.html"
      }
    , { by = "thunderbong"
      , id = 38139251
      , score = 193
      , time = Time.millisToPosix (1699088242 * 1000)
      , title = "Is there anywhere in the world someone can just live for free?"
      , type_ = "story"
      , url = Just "https://reddit.com/r/NoStupidQuestions/comments/17mvukd/is_there_anywhere_in_the_world_someone_can_just/"
      }
    , { by = "sintax"
      , id = 38135619
      , score = 84
      , time = Time.millisToPosix (1699049018 * 1000)
      , title = "First Handset with MTE on the Market"
      , type_ = "story"
      , url = Just "https://googleprojectzero.blogspot.com/2023/11/first-handset-with-mte-on-market.html"
      }
    , { by = "bookofjoe"
      , id = 38145402
      , score = 59
      , time = Time.millisToPosix (1699133880 * 1000)
      , title = "Cats Make Nearly 300 Different Facial Expressions"
      , type_ = "story"
      , url = Just "https://www.smithsonianmag.com/smart-news/cats-make-nearly-300-different-facial-expressions-180983185/"
      }
    , { by = "notmysql_"
      , id = 38123526
      , score = 147
      , time = Time.millisToPosix (1698977455 * 1000)
      , title = "Email.radio – Free Email Domain for Licensed Ham Radio Operators"
      , type_ = "story"
      , url = Just "https://email.radio/"
      }
    , { by = "ColoursofOSINT"
      , id = 38089342
      , score = 666
      , time = Time.millisToPosix (1698777232 * 1000)
      , title = "macOS Sonoma Boot Failures"
      , type_ = "story"
      , url = Just "https://github.com/AsahiLinux/docs/wiki/macOS-Sonoma-Boot-Failures"
      }
    , { by = "threePointFive"
      , id = 38136404
      , score = 98
      , time = Time.millisToPosix (1699054151 * 1000)
      , title = "Telnet BBS Guide"
      , type_ = "story"
      , url = Just "https://www.telnetbbsguide.com/"
      }
    , { by = "leotravis10"
      , id = 38144158
      , score = 69
      , time = Time.millisToPosix (1699125064 * 1000)
      , title = "Discord will switch to temporary file links to block malware delivery"
      , type_ = "story"
      , url = Just "https://www.bleepingcomputer.com/news/security/discord-will-switch-to-temporary-file-links-to-block-malware-delivery/"
      }
    , { by = "mairindubh"
      , id = 38133963
      , score = 201
      , time = Time.millisToPosix (1699040343 * 1000)
      , title = "Podman Desktop v1.5 with Compose onboarding and enhanced Kubernetes pod data"
      , type_ = "story"
      , url = Just "https://podman-desktop.io/blog/podman-desktop-release-1.5"
      }
    , { by = "Brajeshwar"
      , id = 38130870
      , score = 199
      , time = Time.millisToPosix (1699028113 * 1000)
      , title = "Okta hit by third-party breach, stealing employee data"
      , type_ = "story"
      , url = Just "https://arstechnica.com/security/2023/11/okta-hit-by-another-breach-this-one-stealing-employee-data-from-3rd-party-vendor/"
      }
    , { by = "Petiver"
      , id = 38122465
      , score = 124
      , time = Time.millisToPosix (1698970868 * 1000)
      , title = "If you plant milkweed, they will come"
      , type_ = "story"
      , url = Just "https://www.nytimes.com/2023/10/25/realestate/gardening-milkweed-plants-butterflies.html"
      }
    , { by = "henrik_w"
      , id = 38142632
      , score = 31
      , time = Time.millisToPosix (1699115914 * 1000)
      , title = "Things I like about Zig as a Go Programmer"
      , type_ = "story"
      , url = Just "https://medium.com/@vladimirvivien/things-i-like-about-zig-as-a-go-programmer-75eb02aab00f"
      }
    , { by = "jandeboevrie"
      , id = 38136590
      , score = 9
      , time = Time.millisToPosix (1699055875 * 1000)
      , title = "Upgrading my Chumby 8 kernel part 7: touchscreen"
      , type_ = "story"
      , url = Just "https://www.downtowndougbrown.com/2023/11/upgrading-my-chumby-8-kernel-part-7-touchscreen/"
      }
    , { by = "sizzle"
      , id = 38134181
      , score = 114
      , time = Time.millisToPosix (1699041510 * 1000)
      , title = "Embracing community helps us live longer, and be happier (2017)"
      , type_ = "story"
      , url = Just "https://news.harvard.edu/gazette/story/2017/04/over-nearly-80-years-harvard-study-has-been-showing-how-to-live-a-healthy-and-happy-life/"
      }
    , { by = "sjm217"
      , id = 38126997
      , score = 221
      , time = Time.millisToPosix (1699009722 * 1000)
      , title = "Joint statement of scientists and NGOs on the EU’s proposed eIDAS reform"
      , type_ = "story"
      , url = Just "https://eidas-open-letter.org/"
      }
    , { by = "knight17"
      , id = 38140012
      , score = 3
      , time = Time.millisToPosix (1699096900 * 1000)
      , title = "A guide to home row mods with QMK"
      , type_ = "story"
      , url = Just "https://precondition.github.io/home-row-mods"
      }
    , { by = "pella"
      , id = 38134257
      , score = 14
      , time = Time.millisToPosix (1699041813 * 1000)
      , title = "Geospatial Technology Radar 2023"
      , type_ = "story"
      , url = Just "https://element84.com/geospatial-tech-radar-23"
      }
    , { by = "tomcam"
      , id = 38138637
      , score = 49
      , time = Time.millisToPosix (1699078799 * 1000)
      , title = "HBO CEO Confesses to Trolling Critics with Fake Tweets: Rolling Stone"
      , type_ = "story"
      , url = Just "https://www.rollingstone.com/tv-movies/tv-movie-news/hbo-ceo-casey-bloys-confesses-trolling-critics-fake-twitter-accounts-dumb-idea-1234868710/"
      }
    , { by = "Stratoscope"
      , id = 38124998
      , score = 124
      , time = Time.millisToPosix (1698990518 * 1000)
      , title = "Ken Mattingly, astronaut scrubbed from Apollo 13, has died"
      , type_ = "story"
      , url = Just "https://www.nytimes.com/2023/11/02/science/space/ken-mattingly-dead.html"
      }
    , { by = "rbanffy"
      , id = 38120497
      , score = 146
      , time = Time.millisToPosix (1698960829 * 1000)
      , title = "How big data carried graph theory into new dimensions"
      , type_ = "story"
      , url = Just "https://www.quantamagazine.org/how-big-data-carried-graph-theory-into-new-dimensions-20210819/"
      }
    , { by = "oppodeldoc"
      , id = 38106461
      , score = 235
      , time = Time.millisToPosix (1698878878 * 1000)
      , title = "Lemming Suicide Myth: Disney Film Faked Bogus Behavior"
      , type_ = "story"
      , url = Just "https://www.adfg.alaska.gov/index.cfm?adfg=wildlifenews.view_article&articles_id=56"
      }
    , { by = "mrzool"
      , id = 38133632
      , score = 22
      , time = Time.millisToPosix (1699038680 * 1000)
      , title = "The Future of RSS Is Textcasting"
      , type_ = "story"
      , url = Just "https://kottke.org/23/11/the-future-of-rss-is-textcasting-1"
      }
    , { by = "momirlan"
      , id = 38121199
      , score = 113
      , time = Time.millisToPosix (1698964009 * 1000)
      , title = "Auto industry executives admit electric vehicle plans are in jeopardy"
      , type_ = "story"
      , url = Just "https://www.businessinsider.com/auto-executives-coming-clean-evs-arent-working-2023-10"
      }
    , { by = "soheilpro"
      , id = 38112951
      , score = 591
      , time = Time.millisToPosix (1698930333 * 1000)
      , title = "Confusing Git Terminology"
      , type_ = "story"
      , url = Just "https://jvns.ca/blog/2023/11/01/confusing-git-terminology/"
      }
    , { by = "hackerbeat"
      , id = 38089247
      , score = 645
      , time = Time.millisToPosix (1698776835 * 1000)
      , title = "Copying Angry Birds with nothing but AI"
      , type_ = "story"
      , url = Just "https://twitter.com/javilopen/status/1719363262179938401"
      }
    , { by = "signa11"
      , id = 38121765
      , score = 146
      , time = Time.millisToPosix (1698966832 * 1000)
      , title = "Hyphens, minus, and dashes in Debian man pages"
      , type_ = "story"
      , url = Just "https://lwn.net/Articles/947941/"
      }
    , { by = "PaulHoule"
      , id = 38127629
      , score = 91
      , time = Time.millisToPosix (1699014080 * 1000)
      , title = "The Anthropogenic Salt Cycle"
      , type_ = "story"
      , url = Just "https://www.nature.com/articles/s43017-023-00485-y"
      }
    , { by = "jawns"
      , id = 38140390
      , score = 117
      , time = Time.millisToPosix (1699100658 * 1000)
      , title = "When to Shut Up: A Visual Guide (With Included Algorithm)"
      , type_ = "story"
      , url = Just "https://shaungallagher.pressbin.com/blog/shut-up.html"
      }
    , { by = "cstrahan"
      , id = 38104554
      , score = 297
      , time = Time.millisToPosix (1698870262 * 1000)
      , title = "macOS Sonoma Broke Grep"
      , type_ = "story"
      , url = Just "https://developer.apple.com/forums/thread/738862"
      }
    , { by = "todsacerdoti"
      , id = 38095699
      , score = 335
      , time = Time.millisToPosix (1698826089 * 1000)
      , title = "How Bear does analytics with CSS"
      , type_ = "story"
      , url = Just "https://herman.bearblog.dev/how-bear-does-analytics-with-css/"
      }
    , { by = "bikenaga"
      , id = 38121261
      , score = 87
      , time = Time.millisToPosix (1698964276 * 1000)
      , title = "Everything We Know About 1970s Mainframe RPGs We Can No Longer Play"
      , type_ = "story"
      , url = Just "http://crpgaddict.blogspot.com/2021/06/brief-everything-we-know-about-1970s.html"
      }
    , { by = "efface"
      , id = 38105839
      , score = 200
      , time = Time.millisToPosix (1698875579 * 1000)
      , title = "The business of extracting knowledge from academic publications"
      , type_ = "story"
      , url = Just "https://www.theseedsofscience.pub/p/the-business-of-extracting-knowledge"
      }
    , { by = "lagniappe"
      , id = 38107711
      , score = 244
      , time = Time.millisToPosix (1698887782 * 1000)
      , title = "Window-swap – open a new window somewhere in the world"
      , type_ = "story"
      , url = Just "https://www.window-swap.com/"
      }
    , { by = "cebert"
      , id = 38126840
      , score = 80
      , time = Time.millisToPosix (1699008771 * 1000)
      , title = "AMD Unveils Ryzen Mobile 7040U Series with Zen 4c Efficiency Cores"
      , type_ = "story"
      , url = Just "https://www.anandtech.com/show/21111/amd-unveils-ryzen-7040u-series-with-zen-4c-smaller-cores-bigger-efficiency"
      }
    , { by = "thunderbong"
      , id = 38137917
      , score = 231
      , time = Time.millisToPosix (1699068894 * 1000)
      , title = "YouTube's plan backfires, people are installing better ad blockers"
      , type_ = "story"
      , url = Just "https://www.androidauthority.com/youtube-ad-block-installs-3382289/"
      }
    , { by = "greenburger"
      , id = 38142209
      , score = 56
      , time = Time.millisToPosix (1699113368 * 1000)
      , title = "A carbohydrate revolution is speeding up pro cycling"
      , type_ = "story"
      , url = Just "https://velo.outsideonline.com/road/road-training/a-massive-change-how-a-carbohydrate-revolution-is-speeding-up-pro-cycling/"
      }
    , { by = "transpute"
      , id = 38138908
      , score = 30
      , time = Time.millisToPosix (1699082923 * 1000)
      , title = "Intel's CEO Admits to and Details the Company's Three Biggest Mistakes"
      , type_ = "story"
      , url = Just "https://www.tomshardware.com/pc-components/gpus/intels-ceo-admits-to-and-details-companies-three-biggest-mistakes"
      }
    , { by = "midnightdiesel"
      , id = 38145062
      , score = 89
      , time = Time.millisToPosix (1699131540 * 1000)
      , title = "GM's Cruise alleged to rely on human operators to achieve \"autonomous\" driving"
      , type_ = "story"
      , url = Just "https://www.nytimes.com/2023/11/03/technology/cruise-general-motors-self-driving-cars.html"
      }
    , { by = "donpott"
      , id = 38130418
      , score = 674
      , time = Time.millisToPosix (1699025931 * 1000)
      , title = "I trapped scammers in an impossible maze [video]"
      , type_ = "story"
      , url = Just "https://www.youtube.com/watch?v=dWzz3NeDz3E"
      }
    , { by = "franze"
      , id = 38127131
      , score = 64
      , time = Time.millisToPosix (1699010723 * 1000)
      , title = "999 Request Denied"
      , type_ = "story"
      , url = Just "https://http.dev/999"
      }
    , { by = "mnot"
      , id = 38109494
      , score = 537
      , time = Time.millisToPosix (1698904662 * 1000)
      , title = "Last Chance to fix eIDAS: Secret EU law threatens Internet security"
      , type_ = "story"
      , url = Just "https://last-chance-for-eidas.org/"
      }
    , { by = "kaypee901"
      , id = 38134381
      , score = 148
      , time = Time.millisToPosix (1699042445 * 1000)
      , title = "Microsoft is retiring Visual Studio for Mac in 2024"
      , type_ = "story"
      , url = Just "https://visualstudio.microsoft.com/vs/mac/"
      }
    , { by = "asicsp"
      , id = 38124114
      , score = 83
      , time = Time.millisToPosix (1698982071 * 1000)
      , title = "Translations of Russ Cox's Thompson NFA C Program to Rust"
      , type_ = "story"
      , url = Just "https://github.com/BurntSushi/rsc-regexp"
      }
    , { by = "lxm"
      , id = 38115364
      , score = 74
      , time = Time.millisToPosix (1698940588 * 1000)
      , title = "How to pronounce the trickiest English words"
      , type_ = "story"
      , url = Just "https://www.wsj.com/business/media/how-to-pronounce-english-words-julien-miquel-58581065"
      }
    , { by = "ingve"
      , id = 38095276
      , score = 262
      , time = Time.millisToPosix (1698821169 * 1000)
      , title = "Why ACPI?"
      , type_ = "story"
      , url = Just "https://mjg59.dreamwidth.org/68350.html"
      }
    , { by = "jeffreyjsatx"
      , id = 38134389
      , score = 49
      , time = Time.millisToPosix (1699042503 * 1000)
      , title = "Show HN: GitInsights – a weekly summary email of your team's GitHub activity"
      , type_ = "story"
      , url = Just "https://gitinsights.io/"
      }
    , { by = "scapecast"
      , id = 38130060
      , score = 96
      , time = Time.millisToPosix (1699024561 * 1000)
      , title = "Jevons paradox"
      , type_ = "story"
      , url = Just "https://en.wikipedia.org/wiki/Jevons_paradox"
      }
    , { by = "selvan"
      , id = 38109020
      , score = 142
      , time = Time.millisToPosix (1698899569 * 1000)
      , title = "WasmGC – Run GC languages such as Kotlin, Java in Chrome browser"
      , type_ = "story"
      , url = Just "https://developer.chrome.com/blog/wasmgc/"
      }
    , { by = "belter"
      , id = 38144229
      , score = 34
      , time = Time.millisToPosix (1699125601 * 1000)
      , title = "China's TikTok users are being told that Aristotle did not exist"
      , type_ = "story"
      , url = Just "https://www.scmp.com/news/china/science/article/3240339/aristotle-becomes-latest-casualty-chinas-narrative-war-west-scholar-questions-philosophers-existence"
      }
    , { by = "jongalloway2"
      , id = 38122396
      , score = 227
      , time = Time.millisToPosix (1698970499 * 1000)
      , title = "Audacity 3.4"
      , type_ = "story"
      , url = Just "https://www.audacityteam.org/blog/audacity-3-4/"
      }
    , { by = "bookofjoe"
      , id = 38098779
      , score = 318
      , time = Time.millisToPosix (1698849513 * 1000)
      , title = "Why doctors in America earn so much"
      , type_ = "story"
      , url = Just "https://www.economist.com/united-states/2023/10/31/why-doctors-in-america-earn-so-much"
      }
    , { by = "omarfarooq"
      , id = 38093353
      , score = 274
      , time = Time.millisToPosix (1698800141 * 1000)
      , title = "Distil-Whisper: distilled version of Whisper that is 6 times faster, 49% smaller"
      , type_ = "story"
      , url = Just "https://github.com/huggingface/distil-whisper"
      }
    , { by = "magoghm"
      , id = 38146146
      , score = 11
      , time = Time.millisToPosix (1699139127 * 1000)
      , title = "Pessimists Archive"
      , type_ = "story"
      , url = Just "https://pessimistsarchive.org/"
      }
    , { by = "gzer0"
      , id = 38130309
      , score = 202
      , time = Time.millisToPosix (1699025430 * 1000)
      , title = "GPT-4 Update: 32K Context Window Now for All Users"
      , type_ = "story"
      , url = Just "https://github.com/spdustin/ChatGPT-AutoExpert/blob/main/_system-prompts/all_tools.md"
      }
    , { by = "shcheklein"
      , id = 38120493
      , score = 108
      , time = Time.millisToPosix (1698960803 * 1000)
      , title = "ML Experiments Management with Git"
      , type_ = "story"
      , url = Just "https://github.com/iterative/dvc"
      }
    , { by = "ossusermivami"
      , id = 38136155
      , score = 35
      , time = Time.millisToPosix (1699052157 * 1000)
      , title = "You don't need a terminal emulator · Andrey Listopadov"
      , type_ = "story"
      , url = Just "https://andreyor.st/posts/2023-10-27-you-dont-need-a-terminal-emulator/"
      }
    , { by = "voisin"
      , id = 38136314
      , score = 250
      , time = Time.millisToPosix (1699053413 * 1000)
      , title = "Bayer hit with $332M judgement in Roundup cancer trial"
      , type_ = "story"
      , url = Just "https://fortune.com/2023/11/03/bayer-332-million-judgement-trial-man-cancer-decades-monsanto-roundup-weedkiller/"
      }
    , { by = "nolist_policy"
      , id = 38143690
      , score = 58
      , time = Time.millisToPosix (1699121799 * 1000)
      , title = "Openela releases Red Hat source code on GitHub"
      , type_ = "story"
      , url = Just "https://github.com/orgs/openela-main/repositories"
      }
    , { by = "olestr"
      , id = 38126566
      , score = 12
      , time = Time.millisToPosix (1699005947 * 1000)
      , title = "Deno 1.38: HTML doc generator and HMR"
      , type_ = "story"
      , url = Just "https://deno.com/blog/v1.38"
      }
    , { by = "mrngilles"
      , id = 38140812
      , score = 8
      , time = Time.millisToPosix (1699104443 * 1000)
      , title = "Ask HN: Infra people in small companies, what does your infra look like?"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "weird_user"
      , id = 38131460
      , score = 137
      , time = Time.millisToPosix (1699030348 * 1000)
      , title = "Nuclear Conversion for Starship"
      , type_ = "story"
      , url = Just "http://toughsf.blogspot.com/2021/10/nuclear-conversion-for-starship.html"
      }
    , { by = "zdw"
      , id = 38103616
      , score = 206
      , time = Time.millisToPosix (1698867036 * 1000)
      , title = "Now you're thinking with relays"
      , type_ = "story"
      , url = Just "https://lcamtuf.substack.com/p/now-youre-thinking-with-relays"
      }
    , { by = "xrd"
      , id = 38100541
      , score = 158
      , time = Time.millisToPosix (1698855712 * 1000)
      , title = "Bank financing and bad urban planning make the retail apocalypse worse"
      , type_ = "story"
      , url = Just "https://www.businessinsider.com/bank-financing-urban-planning-pandemic-retail-apocalypse-vacant-storefront-2023-10"
      }
    , { by = "behnamoh"
      , id = 38137463
      , score = 31
      , time = Time.millisToPosix (1699064107 * 1000)
      , title = "How much it costs Apple to increase RAM size in Macs?"
      , type_ = "story"
      , url = Just "https://old.reddit.com/r/macbookpro/comments/17lwl0b/how_much_does_ram_cost_anyways/"
      }
    , { by = "bookofjoe"
      , id = 38143578
      , score = 30
      , time = Time.millisToPosix (1699121054 * 1000)
      , title = "Cup Noodles are now microwave-safe"
      , type_ = "story"
      , url = Just "https://www.washingtonpost.com/food/2023/11/03/cup-noodles-are-now-microwave-safe-so-they-werent-already/"
      }
    , { by = "johhns4"
      , id = 38126764
      , score = 30
      , time = Time.millisToPosix (1699008086 * 1000)
      , title = "What's the difference between LangChain, llama indexand others like autollm?"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "gregorymichael"
      , id = 38120097
      , score = 235
      , time = Time.millisToPosix (1698958954 * 1000)
      , title = "Sally Ignore Previous Instructions"
      , type_ = "story"
      , url = Just "https://www.haihai.ai/pen15/"
      }
    , { by = "luu"
      , id = 38121918
      , score = 81
      , time = Time.millisToPosix (1698967913 * 1000)
      , title = "Enterprise-Fu"
      , type_ = "story"
      , url = Just "http://mike.daless.io/prez/enterprise-asbury-2013-10/slides.html"
      }
    , { by = "stanulilic"
      , id = 38127560
      , score = 251
      , time = Time.millisToPosix (1699013703 * 1000)
      , title = "Rwanda joins Kenya to initiate visa-free travel for all Africans"
      , type_ = "story"
      , url = Just "https://africa.businessinsider.com/local/leaders/from-barriers-to-unity-rwanda-joins-kenya-to-initiate-visa-free-travel-for-all/hhqe29q"
      }
    , { by = "g42gregory"
      , id = 38108873
      , score = 792
      , time = Time.millisToPosix (1698897921 * 1000)
      , title = "Yann LeCun: AI one-percenters seizing power forever is real doomsday scenario"
      , type_ = "story"
      , url = Just "https://www.businessinsider.com/sam-altman-and-demis-hassabis-just-want-to-control-ai-2023-10"
      }
    , { by = "Hydrocharged"
      , id = 38108044
      , score = 161
      , time = Time.millisToPosix (1698890374 * 1000)
      , title = "Show HN: DoltgreSQL – Version-Controlled DB, Like Git and PostgreSQL had a baby"
      , type_ = "story"
      , url = Just "https://github.com/dolthub/doltgresql"
      }
    , { by = "basic_banana"
      , id = 38126160
      , score = 74
      , time = Time.millisToPosix (1699002078 * 1000)
      , title = "Clash, a tool to break China's Great Fire Wall, is Deleted in GitHub"
      , type_ = "story"
      , url = Just "https://github.com/Dreamacro/clash"
      }
    , { by = "sbt567"
      , id = 38108564
      , score = 40
      , time = Time.millisToPosix (1698894868 * 1000)
      , title = "Vivo Unveils BlueOS, Based on Rust Language"
      , type_ = "story"
      , url = Just "https://sparrowsnews.com/2023/11/01/vivo-unveils-blueos-operating-system/"
      }
    , { by = "bindidwodtj"
      , id = 38109327
      , score = 66
      , time = Time.millisToPosix (1698902867 * 1000)
      , title = "Formalizing 100 Theorems"
      , type_ = "story"
      , url = Just "https://www.cs.ru.nl/~freek/100/"
      }
    , { by = "qsantos"
      , id = 38125367
      , score = 184
      , time = Time.millisToPosix (1698994406 * 1000)
      , title = "The Binder Linux driver is being rewritten in Rust"
      , type_ = "story"
      , url = Just "https://lore.kernel.org/rust-for-linux/20231101-rust-binder-v1-0-08ba9197f637@google.com/"
      }
    ]


textPosts : List Post
textPosts =
    [ { by = "mNovak"
      , id = 38120388
      , score = 411
      , time = Time.millisToPosix (1698960362 * 1000)
      , title = "Tell HN: Submit comments to IRS re tax treatment of software dev expenses"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "robomartin"
      , id = 38148119
      , score = 5
      , time = Time.millisToPosix (1699157746 * 1000)
      , title = "A game. The only winning move is not to play"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "chen_dev"
      , id = 38146738
      , score = 17
      , time = Time.millisToPosix (1699144434 * 1000)
      , title = "Oracle Cloud Outage"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "lawgimenez"
      , id = 38146887
      , score = 7
      , time = Time.millisToPosix (1699145876 * 1000)
      , title = "Ask HN: Where do you get your health news?"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "whoishiring"
      , id = 38099086
      , score = 481
      , time = Time.millisToPosix (1698850843 * 1000)
      , title = "Ask HN: Who is hiring? (November 2023)"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "mrngilles"
      , id = 38140812
      , score = 8
      , time = Time.millisToPosix (1699104443 * 1000)
      , title = "Ask HN: Infra people in small companies, what does your infra look like?"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "johhns4"
      , id = 38126764
      , score = 30
      , time = Time.millisToPosix (1699008086 * 1000)
      , title = "What's the difference between LangChain, llama indexand others like autollm?"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "ThalesX"
      , id = 38142782
      , score = 95
      , time = Time.millisToPosix (1699116766 * 1000)
      , title = "Ask HN: How would French police locate suspects by tapping their devices?"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "whoishiring"
      , id = 38099084
      , score = 100
      , time = Time.millisToPosix (1698850842 * 1000)
      , title = "Ask HN: Who wants to be hired? (November 2023)"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "divbzero"
      , id = 38124845
      , score = 180
      , time = Time.millisToPosix (1698988895 * 1000)
      , title = "Hidden manuals: gittutorial, giteveryday, gitglossary, gitworkflow"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "estebandalelr"
      , id = 38129293
      , score = 26
      , time = Time.millisToPosix (1699021669 * 1000)
      , title = "Ask HN: How do you learn Rust in 2023?"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "bitzun"
      , id = 38133885
      , score = 8
      , time = Time.millisToPosix (1699039916 * 1000)
      , title = "Ask HN: Any questionable WASM-only websites?"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "graemep"
      , id = 38111299
      , score = 13
      , time = Time.millisToPosix (1698920012 * 1000)
      , title = "Ask HN: Best Practices for Accessible Websites"
      , type_ = "story"
      , url = Nothing
      }
    , { by = "engineerick"
      , id = 38110960
      , score = 15
      , time = Time.millisToPosix (1698917347 * 1000)
      , title = "Ask HN: Seeking Advice – Relocating to Canada as a Software Engineer"
      , type_ = "story"
      , url = Nothing
      }
    ]


jobPosts : List Post
jobPosts =
    [ { by = "pranay01"
      , id = 38148992
      , score = 1
      , time = Time.millisToPosix (1699167669 * 1000)
      , title = "SigNoz (YC W21), open source observability platform, hiring SREs (Remote)"
      , type_ = "job"
      , url = Just "https://signoz.io/careers/site-reliability-engineer/"
      }
    ]
