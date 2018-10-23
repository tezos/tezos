(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let adjectives = [|
  "abandoned";"able";"absolute";"adorable";"adventurous";"academic";"acceptable";
  "acclaimed";"accomplished";"accurate";"aching";"acidic";"acrobatic";"active";
  "actual";"adept";"admirable";"admired";"adolescent";"adorable";"adored";
  "advanced";"afraid";"affectionate";"aged";"aggravating";"aggressive";"agile";
  "agitated";"agonizing";"agreeable";"ajar";"alarmed";"alarming";"alert";
  "alienated";"alive";"all";"altruistic";"amazing";"ambitious";"ample";"amused";
  "amusing";"anchored";"ancient";"angelic";"angry";"anguished";"animated";
  "annual";"another";"antique";"anxious";"any";"apprehensive";"appropriate";
  "apt";"arctic";"arid";"aromatic";"artistic";"ashamed";"assured";"astonishing";
  "athletic";"attached";"attentive";"attractive";"austere";"authentic";
  "authorized";"automatic";"avaricious";"average";"aware";"awesome";"awful";
  "awkward";"babyish";"bad";"back";"baggy";"bare";"barren";"basic";"beautiful";
  "belated";"beloved";"beneficial";"better";"best";"bewitched";"big";
  "biodegradable";"bitter";"black";"bland";"blank";"blaring";"bleak";"blind";
  "blissful";"blond";"blue";"blushing";"bogus";"boiling";"bold";"bony";"boring";
  "bossy";"both";"bouncy";"bountiful";"bowed";"brave";"breakable";"brief";
  "bright";"brilliant";"brisk";"broken";"bronze";"brown";"bruised";"bubbly";
  "bulky";"bumpy";"buoyant";"burdensome";"burly";"bustling";"busy";"buttery";
  "buzzing";"calculating";"calm";"candid";"canine";"capital";"carefree";
  "careful";"careless";"caring";"cautious";"cavernous";"celebrated";"charming";
  "cheap";"cheerful";"cheery";"chief";"chilly";"chubby";"circular";"classic";
  "clean";"clear";"clever";"close";"closed";"cloudy";"clueless";"clumsy";
  "cluttered";"coarse";"cold";"colorful";"colorless";"colossal";"comfortable";
  "common";"compassionate";"competent";"complete";"complex";"complicated";
  "composed";"concerned";"concrete";"confused";"conscious";"considerate";
  "constant";"content";"conventional";"cooked";"cool";"cooperative";
  "coordinated";"corny";"corrupt";"costly";"courageous";"courteous";"crafty";
  "crazy";"creamy";"creative";"creepy";"criminal";"crisp";"critical";"crooked";
  "crowded";"cruel";"crushing";"cuddly";"cultivated";"cultured";"cumbersome";
  "curly";"curvy";"cute";"cylindrical";"damaged";"damp";"dangerous";"dapper";
  "daring";"darling";"dark";"dazzling";"dead";"deadly";"deafening";"dear";
  "dearest";"decent";"decimal";"decisive";"deep";"defenseless";"defensive";
  "defiant";"deficient";"definite";"definitive";"delayed";"delectable";
  "delicious";"delightful";"delirious";"demanding";"dense";"dental";"dependable";
  "dependent";"descriptive";"deserted";"detailed";"determined";"devoted";
  "different";"difficult";"digital";"diligent";"dim";"dimpled";"dimwitted";
  "direct";"disastrous";"discrete";"disfigured";"disgusting";"disloyal";"dismal";
  "distant";"downright";"dreary";"dirty";"disguised";"dishonest";"dismal";
  "distant";"distinct";"distorted";"dizzy";"dopey";"doting";"double";"downright";
  "drab";"drafty";"dramatic";"dreary";"droopy";"dry";"dual";"dull";"dutiful";
  "eager";"earnest";"early";"easy";"ecstatic";"edible";"educated";"elaborate";
  "elastic";"elated";"elderly";"electric";"elegant";"elementary";"elliptical";
  "embarrassed";"embellished";"eminent";"emotional";"empty";"enchanted";
  "enchanting";"energetic";"enlightened";"enormous";"enraged";"entire";"envious";
  "equal";"equatorial";"essential";"esteemed";"ethical";"euphoric";"even";
  "evergreen";"everlasting";"every";"evil";"exalted";"excellent";"exemplary";
  "exhausted";"excitable";"excited";"exciting";"exotic";"expensive";
  "experienced";"expert";"extraneous";"extroverted";"fabulous";"failing";"faint";
  "fair";"faithful";"fake";"false";"familiar";"famous";"fancy";"fantastic";"far";
  "faraway";"fast";"fat";"fatal";"fatherly";"favorable";"favorite";"fearful";
  "fearless";"feisty";"feline";"female";"feminine";"few";"fickle";"filthy";
  "fine";"finished";"firm";"first";"firsthand";"fitting";"fixed";"flaky";
  "flamboyant";"flashy";"flat";"flawed";"flawless";"flickering";"flimsy";
  "flippant";"flowery";"fluffy";"fluid";"flustered";"focused";"fond";"foolhardy";
  "foolish";"forceful";"forked";"formal";"forsaken";"forthright";"fortunate";
  "fragrant";"frail";"frank";"frayed";"free";"french";"fresh";"frequent";
  "friendly";"frightened";"frightening";"frigid";"frilly";"frizzy";"frivolous";
  "front";"frosty";"frozen";"frugal";"fruitful";"full";"fumbling";"functional";
  "funny";"fussy";"fuzzy";"gargantuan";"gaseous";"general";"generous";"gentle";
  "genuine";"giant";"giddy";"gigantic";"gifted";"giving";"glamorous";"glaring";
  "glass";"gleaming";"gleeful";"glistening";"glittering";"gloomy";"glorious";
  "glossy";"glum";"golden";"good";"gorgeous";"graceful";"gracious";"grand";
  "grandiose";"granular";"grateful";"grave";"gray";"great";"greedy";"green";
  "gregarious";"grim";"grimy";"gripping";"grizzled";"gross";"grotesque";
  "grouchy";"grounded";"growing";"growling";"grown";"grubby";"gruesome";"grumpy";
  "guilty";"gullible";"gummy";"hairy";"half";"handmade";"handsome";"handy";
  "happy";"hard";"harmful";"harmless";"harmonious";"harsh";"hasty";"hateful";
  "haunting";"healthy";"heartfelt";"hearty";"heavenly";"heavy";"hefty";"helpful";
  "helpless";"hidden";"hideous";"high";"hilarious";"hoarse";"hollow";"homely";
  "honest";"honorable";"honored";"hopeful";"horrible";"hospitable";"hot";"huge";
  "humble";"humiliating";"humming";"humongous";"hungry";"hurtful";"husky";"icky";
  "icy";"ideal";"idealistic";"identical";"idle";"idiotic";"idolized";"ignorant";
  "ill";"illegal";"illiterate";"illustrious";"imaginary";"imaginative";
  "immaculate";"immaterial";"immediate";"immense";"impassioned";"impeccable";
  "impartial";"imperfect";"imperturbable";"impish";"impolite";"important";
  "impossible";"impractical";"impressionable";"impressive";"improbable";"impure";
  "inborn";"incomparable";"incompatible";"incomplete";"inconsequential";
  "incredible";"indelible";"inexperienced";"indolent";"infamous";"infantile";
  "infatuated";"inferior";"infinite";"informal";"innocent";"insecure";
  "insidious";"insignificant";"insistent";"instructive";"insubstantial";
  "intelligent";"intent";"intentional";"interesting";"internal";"international";
  "intrepid";"ironclad";"irresponsible";"irritating";"itchy";"jaded";"jagged";
  "jaunty";"jealous";"jittery";"joint";"jolly";"jovial";"joyful";"joyous";
  "jubilant";"judicious";"juicy";"jumbo";"junior";"jumpy";"juvenile";
  "kaleidoscopic";"keen";"key";"kind";"kindhearted";"kindly";"klutzy";"knobby";
  "knotty";"knowledgeable";"knowing";"known";"kooky";"lame";"lanky";"large";
  "last";"lasting";"late";"lavish";"lawful";"lazy";"leading";"lean";"leafy";
  "left";"legal";"legitimate";"light";"lighthearted";"likable";"likely";
  "limited";"limp";"limping";"linear";"lined";"liquid";"little";"live";"lively";
  "livid";"loathsome";"lone";"lonely";"long";"loose";"lopsided";"lost";"loud";
  "lovable";"lovely";"loving";"low";"loyal";"lucky";"lumbering";"luminous";
  "lumpy";"lustrous";"luxurious";"mad";"magnificent";"majestic";"major";"male";
  "mammoth";"married";"marvelous";"masculine";"massive";"mature";"meager";
  "mealy";"mean";"measly";"meaty";"medical";"mediocre";"medium";"meek";"mellow";
  "melodic";"memorable";"menacing";"merry";"messy";"metallic";"mild";"milky";
  "mindless";"miniature";"minor";"minty";"miserable";"miserly";"misguided";
  "misty";"mixed";"modern";"modest";"moist";"monstrous";"monthly";"monumental";
  "moral";"mortified";"motherly";"motionless";"mountainous";"muddy";"muffled";
  "multicolored";"mundane";"murky";"mushy";"musty";"muted";"mysterious";"naive";
  "narrow";"nasty";"natural";"naughty";"nautical";"near";"neat";"necessary";
  "needy";"negative";"neglected";"negligible";"neighboring";"nervous";"new";
  "nice";"nifty";"nimble";"nippy";"nocturnal";"noisy";"nonstop";"normal";
  "notable";"noted";"noteworthy";"novel";"noxious";"numb";"nutritious";"nutty";
  "obedient";"obese";"oblong";"oily";"oblong";"obvious";"occasional";"odd";
  "oddball";"offbeat";"offensive";"official";"old";"only";"open";"optimal";
  "optimistic";"opulent";"orange";"orderly";"organic";"ornate";"ornery";
  "ordinary";"original";"other";"our";"outlying";"outgoing";"outlandish";
  "outrageous";"outstanding";"oval";"overcooked";"overdue";"overjoyed";
  "overlooked";"palatable";"pale";"paltry";"parallel";"parched";"partial";
  "passionate";"past";"pastel";"peaceful";"peppery";"perfect";"perfumed";
  "periodic";"perky";"personal";"pertinent";"pesky";"pessimistic";"petty";
  "phony";"physical";"piercing";"pink";"pitiful";"plain";"plaintive";"plastic";
  "playful";"pleasant";"pleased";"pleasing";"plump";"plush";"polished";"polite";
  "political";"pointed";"pointless";"poised";"poor";"popular";"portly";"posh";
  "positive";"possible";"potable";"powerful";"powerless";"practical";"precious";
  "present";"prestigious";"pretty";"precious";"previous";"pricey";"prickly";
  "primary";"prime";"pristine";"private";"prize";"probable";"productive";
  "profitable";"profuse";"proper";"proud";"prudent";"punctual";"pungent";"puny";
  "pure";"purple";"pushy";"putrid";"puzzled";"puzzling";"quaint";"qualified";
  "quarrelsome";"quarterly";"queasy";"querulous";"questionable";"quick";"quiet";
  "quintessential";"quirky";"quixotic";"quizzical";"radiant";"ragged";"rapid";
  "rare";"rash";"raw";"recent";"reckless";"rectangular";"ready";"real";
  "realistic";"reasonable";"red";"reflecting";"regal";"regular";"reliable";
  "relieved";"remarkable";"remorseful";"remote";"repentant";"required";
  "respectful";"responsible";"repulsive";"revolving";"rewarding";"rich";"rigid";
  "right";"ringed";"ripe";"roasted";"robust";"rosy";"rotating";"rotten";"rough";
  "round";"rowdy";"royal";"rubbery";"rundown";"ruddy";"rude";"runny";"rural";
  "rusty";"sad";"safe";"salty";"same";"sandy";"sane";"sarcastic";"sardonic";
  "satisfied";"scaly";"scarce";"scared";"scary";"scented";"scholarly";
  "scientific";"scornful";"scratchy";"scrawny";"second";"secondary";"secret";
  "selfish";"sentimental";"separate";"serene";"serious";"serpentine";"several";
  "severe";"shabby";"shadowy";"shady";"shallow";"shameful";"shameless";"sharp";
  "shimmering";"shiny";"shocked";"shocking";"shoddy";"short";"showy";"shrill";
  "shy";"sick";"silent";"silky";"silly";"silver";"similar";"simple";"simplistic";
  "sinful";"single";"sizzling";"skeletal";"skinny";"sleepy";"slight";"slim";
  "slimy";"slippery";"slow";"slushy";"small";"smart";"smoggy";"smooth";"smug";
  "snappy";"snarling";"sneaky";"sniveling";"snoopy";"sociable";"soft";"soggy";
  "solid";"somber";"some";"spherical";"sophisticated";"sore";"sorrowful";
  "soulful";"soupy";"sour";"spanish";"sparkling";"sparse";"specific";
  "spectacular";"speedy";"spicy";"spiffy";"spirited";"spiteful";"splendid";
  "spotless";"spotted";"spry";"square";"squeaky";"squiggly";"stable";"staid";
  "stained";"stale";"standard";"starchy";"stark";"starry";"steep";"sticky";
  "stiff";"stimulating";"stingy";"stormy";"straight";"strange";"steel";"strict";
  "strident";"striking";"striped";"strong";"studious";"stunning";"stupendous";
  "stupid";"sturdy";"stylish";"subdued";"submissive";"substantial";"subtle";
  "suburban";"sudden";"sugary";"sunny";"super";"superb";"superficial";"superior";
  "supportive";"surprised";"suspicious";"svelte";"sweaty";"sweet";"sweltering";
  "swift";"sympathetic";"tall";"talkative";"tame";"tan";"tangible";"tart";
  "tasty";"tattered";"taut";"tedious";"teeming";"tempting";"tender";"tense";
  "tepid";"terrible";"terrific";"testy";"thankful";"that";"these";"thick";"thin";
  "third";"thirsty";"this";"thorough";"thorny";"those";"thoughtful";"threadbare";
  "thrifty";"thunderous";"tidy";"tight";"timely";"tinted";"tiny";"tired";"torn";
  "total";"tough";"traumatic";"treasured";"tremendous";"tragic";"trained";
  "tremendous";"triangular";"tricky";"trifling";"trim";"trivial";"troubled";
  "true";"trusting";"trustworthy";"trusty";"truthful";"tubby";"turbulent";"twin";
  "ugly";"ultimate";"unacceptable";"unaware";"uncomfortable";"uncommon";
  "unconscious";"understated";"unequaled";"uneven";"unfinished";"unfit";
  "unfolded";"unfortunate";"unhappy";"unhealthy";"uniform";"unimportant";
  "unique";"united";"unkempt";"unknown";"unlawful";"unlined";"unlucky";
  "unnatural";"unpleasant";"unrealistic";"unripe";"unruly";"unselfish";
  "unsightly";"unsteady";"unsung";"untidy";"untimely";"untried";"untrue";
  "unused";"unusual";"unwelcome";"unwieldy";"unwilling";"unwitting";"unwritten";
  "upbeat";"upright";"upset";"urban";"usable";"used";"useful";"useless";
  "utilized";"utter";"vacant";"vague";"vain";"valid";"valuable";"vapid";
  "variable";"vast";"velvety";"venerated";"vengeful";"verifiable";"vibrant";
  "vicious";"victorious";"vigilant";"vigorous";"villainous";"violet";"violent";
  "virtual";"virtuous";"visible";"vital";"vivacious";"vivid";"voluminous";
  "warlike";"warm";"warmhearted";"warped";"wary";"wasteful";"watchful";
  "waterlogged";"watery";"wavy";"wealthy";"weak";"weary";"webbed";"wee";"weekly";
  "weepy";"weighty";"weird";"welcome";"wet";"which";"whimsical";"whirlwind";
  "whispered";"white";"whole";"whopping";"wicked";"wide";"wiggly";"wild";
  "willing";"wilted";"winding";"windy";"winged";"wiry";"wise";"witty";"wobbly";
  "woeful";"wonderful";"wooden";"woozy";"wordy";"worldly";"worn";"worried";
  "worrisome";"worse";"worst";"worthless";"worthwhile";"worthy";"wrathful";
  "wretched";"writhing";"wrong";"wry";"yawning";"yearly";"yellow";"yellowish";
  "young";"youthful";"yummy";"zany";"zealous";"zesty";
|]

let animals = [|
  "aardvark";"abyssinian";"affenpinscher";"akbash";"akita";"albatross";
  "alligator";"angelfish";"ant";"anteater";"antelope";"armadillo";"avocet";
  "axolotl";"baboon";"badger";"balinese";"bandicoot";"barb";"barnacle";
  "barracuda";"bat";"beagle";"bear";"beaver";"beetle";"binturong";"birman";
  "bison";"bloodhound";"bobcat";"bombay";"bongo";"bonobo";"booby";"budgerigar";
  "buffalo";"bulldog";"bullfrog";"burmese";"butterfly";"caiman";"camel";
  "capybara";"caracal";"cassowary";"cat";"caterpillar";"catfish";"centipede";
  "chameleon";"chamois";"cheetah";"chicken";"chihuahua";"chimpanzee";
  "chinchilla";"chinook";"chipmunk";"cichlid";"coati";"cockroach";"collie";
  "coral";"cougar";"cow";"coyote";"crab";"crane";"crocodile";"cuscus";
  "cuttlefish";"dachshund";"dalmatian";"deer";"dhole";"dingo";"discus";"dodo";
  "dog";"dolphin";"donkey";"dormouse";"dragonfly";"drever";"duck";"dugong";
  "dunker";"eagle";"earwig";"echidna";"elephant";"emu";"falcon";"fennec";
  "ferret";"fish";"flamingo";"flounder";"fly";"fossa";"fox";"frigatebird";"frog";
  "gar";"gecko";"gerbil";"gharial";"gibbon";"giraffe";"goat";"goose";"gopher";
  "gorilla";"grasshopper";"greyhound";"grouse";"guppy";"hamster";"hare";
  "harrier";"havanese";"hedgehog";"heron";"himalayan";"hippopotamus";"horse";
  "human";"hummingbird";"hyena";"ibis";"iguana";"impala";"indri";"insect";
  "jackal";"jaguar";"javanese";"jellyfish";"kakapo";"kangaroo";"kingfisher";
  "kiwi";"koala";"kudu";"labradoodle";"ladybird";"lemming";"lemur";"leopard";
  "liger";"lion";"lionfish";"lizard";"llama";"lobster";"lynx";"macaw";"magpie";
  "maltese";"manatee";"mandrill";"markhor";"mastiff";"mayfly";"meerkat";
  "millipede";"mole";"molly";"mongoose";"mongrel";"monkey";"moorhen";"moose";
  "moth";"mouse";"mule";"neanderthal";"newfoundland";"newt";"nightingale";
  "numbat";"ocelot";"octopus";"okapi";"olm";"opossum";"ostrich";"otter";"oyster";
  "pademelon";"panther";"parrot";"peacock";"pekingese";"pelican";"penguin";
  "persian";"pheasant";"pig";"pika";"pike";"piranha";"platypus";"pointer";
  "poodle";"porcupine";"possum";"prawn";"puffin";"pug";"puma";"quail";"quetzal";
  "quokka";"quoll";"rabbit";"raccoon";"ragdoll";"rat";"rattlesnake";"reindeer";
  "rhinoceros";"robin";"rottweiler";"salamander";"saola";"scorpion";"seahorse";
  "seal";"serval";"sheep";"shrimp";"siamese";"siberian";"skunk";"sloth";"snail";
  "snake";"snowshoe";"somali";"sparrow";"sponge";"squid";"squirrel";"starfish";
  "stingray";"stoat";"swan";"tang";"tapir";"tarsier";"termite";"tetra";"tiffany";
  "tiger";"tortoise";"toucan";"tropicbird";"tuatara";"turkey";"uakari";"uguisu";
  "umbrellabird";"vulture";"wallaby";"walrus";"warthog";"wasp";"weasel";
  "whippet";"wildebeest";"wolf";"wolverine";"wombat";"woodlouse";"woodpecker";
  "wrasse";"yak";"zebra";"zebu";"zonkey";"zorse";
|]

let pick a z =
  a.(Z.rem z (Array.length a |> Z.of_int) |> Z.to_int)

let hash a =
  Blake2B.hash_string [a] |> Blake2B.to_string

type t = {
  c : string ;
  t : string ;
  h : string ;
  d : string ;
}

let pp ppf { c ; t ; h ; d } =
  Format.fprintf ppf "%s-%s-%s-%s" c t h d

let crouching_tiger string =
  let c = pick adjectives (string |> hash |> Z.of_bits) in
  let t = pick animals    (string |> hash |> hash |> Z.of_bits) in
  let h = pick adjectives (string |> hash |> hash |> hash |> Z.of_bits) in
  let d = pick animals    (string |> hash |> hash |> hash |> hash |> Z.of_bits) in
  { c ; t ; h ; d }
