module Utils where

import Data.List (intersperse, concat, foldl')
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Control.Monad

import Data.Vector.Unboxed (create, freeze)
import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as MUV
import System.Random

import Control.Monad.Random
import System.Random.Mersenne.Pure64

-- ** GENERAL GAME DATA TYPES **

data Player = Black | White
    deriving (Eq, Show)

data Figure = Man | King
    deriving (Eq, Show)

data Square = Empty | Tile Player Figure
    deriving Eq

type Position = (Int, Int)

type Weight = Double

data VectorBoard = VectorBoard (V.Vector (V.Vector Square))
    deriving Eq

data GameState = GameState VectorBoard Player
    deriving Eq


-- ** EVOLUTIONARY DATA TYPES **

type Genome a = [a]

-- type FitnessFun a = Genome a -> Int

type SelectionFun a = [(Genome a, Double)] -> Rand PureMT [Genome a]

type Crossover a = (Genome a, Genome a) -> Rand PureMT (Genome a, Genome a)

type Mutation a = Genome a -> Rand PureMT (Genome a)



-- ** BOARD INSTANCES

instance Show Square where
    show Empty = "."
    show (Tile Black Man) = "b"
    show (Tile Black King) = "B"
    show (Tile White Man) = "w"
    show (Tile White King) = "W"


instance Show VectorBoard where
    show board = unlines ([colIndex] ++ boardStr) where
        -- display the column index at the top
        colIndex = concat (intersperse " | " $ (id " ") : [ [x] | x <- ['0'..'7'] ]) ++ " |"
        -- display each row along with its index
        boardStr = zipWith showRow [0..7] $ boardToList board
        -- HELPERS
        --  convert board to list of squares
        boardToList (VectorBoard b) = V.toList $ V.map V.toList b
        -- display a single board row
        showRow i sqs = concat (intersperse " | " $ (show i) : (map show sqs) ) ++ " |"


initialBoard :: VectorBoard
-- each elem in the concat is of type V.Vector (V.Vector Square)
initialBoard = VectorBoard $ V.concat[V.replicate 1 (initialOddRow blackrow),
                        V.replicate 1 (initialEvenRow blackrow),
                        V.replicate 1 (initialOddRow blackrow),
                        V.replicate 2 emptyrow, 
                        V.replicate 1 (initialEvenRow whiterow),
                        V.replicate 1 (initialOddRow whiterow),
                        V.replicate 1 (initialEvenRow whiterow)]
                    where
                        blackrow = V.replicate 8 (Tile Black Man)
                        whiterow = V.replicate 8 (Tile White Man)
                        emptyrow = V.replicate 8 Empty
                        -- generate rows 1,3 and 7
                        initialOddRow vect = vect V.// [(i, Empty) | i <- [0..7], even i]
                        -- generate rows 2,6 and 8
                        initialEvenRow vect = vect V.// [(i, Empty) | i <- [0..7], odd i]

instance Show GameState where
    show (GameState board player) = 
        show board ++ show player



-- ** BOARD FUNCTIONS **

-- convert a position in a 2D array in the row, column format to a 1D index; zero-indexed
convertPos2Index :: Position -> Int
convertPos2Index (row, col) = row * 8 + col



-- ** MOVE AUXILIARY FUNCTIONS **


-- Convert a jump triple in the form of (origin, (destination, inbetween)) 
-- to (origin, inbetween, destination)
flattenJump :: (Position, (Position, Position)) -> (Position, Position, Position)
flattenJump (a,(b,c)) = (a,c,b)

-- Given an origin and destination position tuple and a list of triplets of jump positions
-- in the form of (origin, (destination, inbetween)) find the tuple in the list and 
-- extract the position of the square inbetween the origin and destination if it exists
getInbetweenPosition :: (Position, Position) -> [(Position, (Position, Position))] -> Maybe Position
getInbetweenPosition _ [] = Nothing
getInbetweenPosition (orig, dest) ((o,(d,i)):ps) 
    | orig == o && dest == d = Just i
    | otherwise = getInbetweenPosition (orig, dest) ps

-- Check if a both elements of tuple of origin and destination positions are members of a list of triplets
elemTriplet :: (Position, Position) -> [(Position, (Position, Position))] -> Bool
elemTriplet _ [] = False
elemTriplet (x,y) ((k,(l,_)):ps)
    | x == k && y == l = True
    | otherwise = elemTriplet (x,y) ps


-- ***************************
-- ***** WEIGHTED PIECES *****
-- ***************************

-- Assign to each piece a value, used in the evaluation of each board
-- positive for White
-- negative for Black
pieceVal :: Square -> Double
pieceVal (Tile Black Man) = 1.0
pieceVal (Tile Black King) = 1.3
pieceVal (Tile White Man) = -1.0
pieceVal (Tile White King) = -1.3
pieceVal Empty = 0.0


-- ** GENERAL GAME PLAYING **

-- Get the opposite player of the one given
oppositeOf :: Player -> Player
oppositeOf White = Black
oppositeOf Black = White


twentyFixedRandomGenomes :: [Genome Double]
twentyFixedRandomGenomes = [[0.29144309749863917,0.4116814340297005,9.582039166458944e-2,0.2680959687520773,0.6419327854883693,0.1250007276742381,0.7472469605478386,0.10602906777495302,8.529015917991634e-2,0.44849386132054636,0.8295266395781095,0.14043534544308545,9.568709744320003e-3,0.20544925330515473,0.7262251680793468,0.5833541458859877,0.6360282970745477,0.8066928816808405,0.30096889489309187,0.11113155447399259,0.7863440993385662,1.6135682446502875e-2,0.12303345053144088,0.25729639730133813,0.4330640069802495,0.5910356619526536,0.8236383658472893,0.13573369412732295,0.656730412762301,0.8546776807809151,0.5094344992554442,0.3365113325353334,0.36991325482435633,9.753158093075387e-2,0.8188082248198109,0.2554065350801715,0.10712295935816063,0.19318932735138705,0.23936869050794785,0.6031070776408762,8.162963875313345e-2,0.7085363274450605,0.6654239485816463,0.9883483453048332,0.9988936122566279,0.8395567011381433,0.21256306768372735,0.720376183283283,0.6918666204818686,0.4221658917047669,0.15273003315429168,0.4965619276879051,0.3090565184482462,0.29886261445461904,0.8697875714473978,0.23137129963627345,0.30884102741475405,0.3234254731741615,8.959338139462625e-2,0.1161290196660748,0.5815067102466013,0.33202968858385384,0.9137154549289165,0.1821322950634663],[0.799159832903468,0.881626423715691,2.5842700749487357e-2,0.3967071886929455,0.505185676940134,0.8727634004759467,0.8888015978466712,0.1809148339208997,0.9073772836927484,0.253393509155348,0.7927708474154016,6.828067419531791e-2,0.6995524432112336,0.9320768874885831,0.2964072203587724,0.8385333777452187,0.363061407528498,0.5946487276512783,0.4020042925833798,0.24863755635152007,0.6194017742137142,4.381636318233462e-2,0.8072201632769708,9.88912885780735e-2,0.43293026342840946,0.42327790252423547,0.4372226411178557,0.3586598435244073,0.9746536636121569,0.23601613937619337,0.24824629454381442,0.34395823314880103,0.7480264074347256,0.9839054755953472,0.21346346616911405,0.7591447355838198,0.3272330145701121,0.61616887478407,0.860755557127807,0.8739534384575082,0.7561138560245948,0.9038941766115042,0.6803400943296203,3.894392545346037e-2,0.5031996462057563,0.6807436029019563,0.3903060310707972,0.6708479727897027,0.6697107819271421,6.1068583511715735e-2,0.7039858363049478,0.6555758346555596,0.5221176220304243,0.5632841300785133,0.7730537656991273,0.6555558271899357,0.5305395093011611,0.43682516438542407,0.9052527385355401,0.7848776748138026,8.767879953289692e-2,0.503712155508813,0.5874451436199765,0.32920920135901366],[0.5946367353695892,0.4953583374325988,0.5901780159101718,1.9726956118119254e-2,0.3145507067023393,0.9529628536733279,0.7833941729996353,0.6196540033379437,0.7896260285685908,0.15609599603374547,0.4308713353824789,0.4506970430296451,0.19401402145510782,0.2093977382975849,0.8746432625670175,0.1889436508268686,0.9766941118242677,0.7424865950411785,0.13200755206100778,0.5485426569097606,0.9387075509458228,0.6735719475999378,0.3229625078336468,0.8060339092621197,0.32425640759394403,0.18493592383638668,0.7638159372868003,0.8285142693275721,0.4613161815586272,0.6722065966626736,0.7900443520926201,0.2146812034195491,0.2837114169490985,0.2958570125769371,0.6118999935950864,0.8890019037366284,5.913040004859371e-3,0.1287075973752283,0.8544886245979865,0.8441746167987969,0.3614805318148189,0.6278407615945694,0.2860132770662748,0.34179648267178275,0.22299081669387422,0.42598489115790394,0.9567599758245114,6.616554717575507e-2,0.7205263094720094,0.10901712555285059,0.7424672949689747,0.5626218555661808,0.3249613600615525,0.9479605068579182,0.25492501454932526,0.35507091135692803,0.9067043238098182,0.9393968947591776,0.9838968148818048,0.24563683305993955,0.6599597490181429,0.9065297513843493,0.7673546170200007,0.3393767753242275],[0.7624011965252663,0.39387738416722196,0.2642032424252535,0.6254185085305193,0.3751140232820003,0.3041282550495372,0.5313498420487205,5.046955669034969e-2,0.11021637069722445,0.6516746678827466,0.7398687886494946,0.998453384580189,4.2101606115424905e-2,0.359944326550554,0.1833665493738248,0.7103783312398071,0.6902413193611734,0.27586874867404776,6.588195145756282e-2,0.32776650352432046,0.764680085243344,0.469197478109564,0.15013937038289515,0.21649096104081755,0.1899596934515444,0.3497862727930763,1.8807756189309854e-2,0.8635403745590625,0.411365145973281,0.6823656003021215,0.2469181759295055,0.5093153026422714,0.4978055625711305,7.128589368566307e-3,0.725572946788292,2.799930339509582e-2,0.44910543198829656,0.6830831713700435,9.749304520093482e-3,0.9167332630712338,0.35945696883192935,0.1341090783756259,0.46862530770872235,0.1325376606872083,0.31928604633155044,0.7289171014567056,0.3897762152518206,0.4677547111685265,0.9597579460514069,0.4875604092662392,0.5881364866083625,0.9316240242862798,0.5523602011895087,0.3648076112557387,0.351549253498712,0.8175953923929005,0.6615974121067598,0.6541337851687026,0.32680391706884515,0.28935777244281713,0.6504464541101166,0.3024674354321162,0.7438664821293185,0.4441323797358935],[0.2964699903289383,0.7834117541458072,0.7088942758924202,0.5017020167156868,0.509198869145145,0.8109263166970221,0.2677087454985143,0.9932891302967447,0.9752781495042617,0.20808719455636138,0.7429312996937685,0.46648938521229866,0.7477165511307768,0.44580595872513606,0.9190135611148822,0.5897474511964078,0.5423430918015932,0.6705282560751831,0.4381799459479333,0.5959790861800023,0.42092506538891994,0.6250128529333865,6.638368418542762e-2,0.2925909054535515,0.39707652149907424,0.31032565921119415,6.494004078957838e-2,0.3212654834158313,0.5251984116231184,0.8031299928301067,0.653646544152586,0.5978635884188305,0.5888128913205026,0.9706468987258403,0.42843147200983234,0.13663401740258174,0.9364762671060917,0.9414710917885855,7.14071872204568e-2,0.4036877471932727,0.7891016356413754,0.32325420319312514,0.6740421994688798,0.2266841985699537,0.1392320889459332,0.45646864561373857,9.376073846404331e-2,0.9995444456849621,0.8811538394863899,0.5550648907543364,9.866483058786313e-2,0.6343592226934067,0.4359698414686445,0.20675953359776011,0.3387191586916579,3.5122568896239215e-2,0.3874938066614574,0.5386920926796949,0.391204438584712,0.446165733970213,0.17371186460901822,3.294188490797978e-2,0.21510906955814046,0.49450689901810674],[0.8053422986593977,0.2752150151674141,0.7529599947754135,0.4615496242493413,0.9654591674400801,0.45516386614660365,0.7261864251888883,0.19937272782512938,0.33111262641114836,8.943637283902572e-3,5.4378828341470786e-2,0.9231177223824739,0.5549287877614499,0.17988006299494985,1.2194841169922865e-2,7.014447548052538e-2,0.7443584824306076,0.3703065499659447,0.7858984665429151,0.6626554633720347,0.16203346734469426,0.4683474796940441,9.976180289573877e-2,0.8572895411462725,0.5885933660700989,0.24974162412131307,0.8165078441882633,0.6406131177998428,0.2602664412877299,0.6523248383930882,0.22898992150106612,0.4647117932690621,0.6122646525721367,1.9023038606400533e-2,0.8813106449919419,0.10835486365463332,0.1607896618085547,0.8633125335000963,0.941582969300631,0.7283430041102184,0.48189813664180303,0.23999638919920385,0.8786464455904546,0.43693214395689084,0.36835572134207284,0.8881160972241723,0.9684858012649172,0.39144561626086494,0.6351626578604096,0.24906099771958212,0.9980387559434217,0.5597664160518285,0.35363351584193514,0.16995249678742963,0.3816042563814369,0.8485336661203207,3.475978723151951e-2,0.3843086228394923,0.14588749488149355,0.9298889345166551,0.2717825116954373,0.1968889615628444,0.8363325062311302,0.10885074471492795],[0.9524237588252319,0.43789554324602653,0.5215356892900082,0.13156527292355036,0.5714805070965596,0.8541065124983788,0.7541056460115897,0.3463252859104279,6.258871970992919e-2,0.30118992102001163,0.3885878651601974,0.5970995532211844,0.9466155691412842,0.27451326262963793,0.47111334037401387,0.9984158374281263,0.812088486249698,0.9236631401125337,0.4052013321969077,0.7230383731505233,0.6096748799866342,6.153405292197489e-2,0.4906336046630465,0.821388329540303,0.9946802107802662,0.9240914083204428,0.3218925160230858,0.6167753869129045,0.38132569386365966,0.20503299180489454,0.10512486113580355,0.5978509895065146,0.1500513314874512,0.904717119447877,0.19818952723076222,0.14392800013148532,0.27151443466088565,0.22196824498299417,0.5767429236475075,5.158241854709533e-2,3.1082079770331728e-2,0.6703063733315376,0.5199246718162132,0.544529536869571,0.15166170972979753,0.8591259019756192,0.14070529365937967,9.444847606810325e-2,0.2414779872087519,0.5235240860108854,0.24469787982009616,0.9610908633674166,0.6606186486892257,3.0759389402251203e-2,0.7387411607139189,0.12265795640417121,0.44078127587440363,0.43698702653140464,0.11758798588348585,0.5570239629460368,0.12180273586260504,0.6292021619991613,0.925640056092791,0.8131818627120878],[7.41249882895576e-2,0.20061093598037294,0.1821935165754226,0.40603737290978914,0.27910747217601284,0.6332189317315096,0.7155362426521348,0.90823322358912,4.622150647327161e-2,0.7081213102597905,0.631779492752661,0.3829057373457685,0.9355804842506263,0.5666310420786171,0.544762613819898,0.3443241968011783,0.804303480743248,0.34674093505986536,0.18966992555363416,0.7615187564766821,0.10385735914540328,0.22699454342729963,7.291491864469313e-3,0.10874667490775691,0.22374343217683623,0.9508064355898344,0.9537222451080213,7.77595816200286e-2,0.24238312101019044,0.34528640393353804,0.1877723361902669,0.8173032863754384,0.9775262050936212,0.6989185294655808,0.7720797612598178,0.5257026870157588,0.6357772257475544,7.168630743333504e-2,0.3618640371258687,0.2898409103817745,0.7490674799025392,0.6865512543677164,0.3303546053047295,0.471521747237841,0.12123585251593305,0.6934751310642107,0.4293348106372227,0.9697172367182351,0.41880778180234446,0.1407250063844132,0.5095554944715939,0.43465295458472,0.28851084333650845,0.32410033906225433,0.7102343644383097,0.16883996344983587,0.23547529276877766,0.3888033241122355,4.3632372627423144e-2,0.8837611226897288,0.9756648222312236,0.8707595062495519,0.744711895377201,0.5510327131934697],[0.7823576099539684,0.8931167147308852,0.49195679592908814,0.8713403879368593,0.17472812007657534,7.48099741367727e-2,0.6275616628516081,0.7313768454948255,0.9721251254277288,2.797329565886153e-2,0.3188377368685956,0.8902700456259195,0.9399800234496515,0.10661829502360498,9.660596390892173e-2,0.9397255693093419,0.5659773323047258,0.971308464177408,0.44945122082156097,0.7458741667406559,0.2652535839760891,0.15545231388651415,0.21123429315244568,0.1711147395590844,0.2830572868716682,0.3489934693541078,0.4332688771746812,0.3594589666906821,0.49179764736590004,0.544422403799369,0.9456719003239272,0.4598106200219134,0.8077127945109892,0.6110945056996053,0.7929332720962603,0.1244060268582422,0.23990440142133895,0.539769519837116,0.9562932182871269,6.0441508152898615e-3,0.4404373957175651,0.4804836281152878,0.23359327028596177,0.7305125789176171,5.9380574117845475e-2,0.9273919481002851,0.6740232270588148,0.5989642495573153,0.2258548675256643,4.467185851628075e-2,0.528325044446762,0.26629515995802444,0.4499517240511468,4.034882181179167e-2,0.8540002630250649,0.8825643241952214,3.7565424381375334e-2,0.956635920208876,0.6705563018597571,0.459566235667604,0.18704218294164776,0.7628716643122763,0.45124118161504323,0.21000265955344533],[0.7523604887676053,0.8530952323549323,0.3286435624057046,0.12485209892872762,0.8840436853177207,0.46594922360118995,0.5498777900425144,0.355950538361001,0.6273333382624573,0.22060105979531852,0.2941531877375506,0.401342546229819,0.7340967912564897,0.6439626251848682,0.452006252177934,0.9797241359574431,7.021567250730831e-2,0.6528191592300188,0.4719929133235392,0.47699246912383897,0.7226686670521196,1.658463440651936e-2,0.35239075967085365,0.4342776830582946,0.9945828506705776,0.7570976475286636,0.6467478957500615,5.539718048849307e-2,0.7989411035298197,0.6474143083486879,0.3508296818528822,5.490577617925607e-2,0.10143459651236164,0.5600330489474581,0.43653534972108865,0.5704404411901864,0.14920633391552207,0.9245939569614275,0.19282473566532365,9.657530646262635e-2,0.7072336943858731,0.8311267010658268,9.224197518900923e-2,0.10157185836064042,0.2218444030209894,0.4673127933559813,0.19524089033297043,0.3754395353249762,0.9255324565714753,0.3032040552303499,0.2729951412251028,2.979585806582108e-2,0.9361031439729262,5.965879377293681e-2,0.22911638524593603,0.26916501165404616,0.504233330847891,0.14189061508295542,0.8236653689026024,0.7361714549358513,0.3643862290616249,0.4692959449807025,0.8749835353019477,0.4548197229684796],[0.8741897393117798,0.37114944721378496,0.5032671582346401,0.4712581403137539,0.8481397448693098,0.4580571223407941,0.8410412495578372,0.10290722016255949,0.692987968411167,0.9326108898337688,5.494785567537763e-2,7.39282602064385e-2,0.871519858090508,0.15866323074154232,0.9566834933494216,0.39155173108526753,8.99854078456025e-2,0.1477107160585882,0.3321400122456136,4.971375692139124e-3,0.25799170054038423,0.39070201435654905,0.9096810265627863,0.649259267135133,0.17921714295748548,0.7736076407482244,0.1426520715532289,0.8659848510258512,0.4350212802414154,0.7069189425019861,0.31835590030615823,0.29728596030533316,0.7474050327391804,0.782551191096365,0.5127424901227894,0.2633601391925502,0.5202617248588761,0.8837728581116759,0.7820490866708795,0.2807962535748647,3.382987158109363e-2,1.6786229178102907e-3,0.19734804992620147,0.12110597313752736,0.9793741799487224,0.5431398528309166,0.9494090839224365,2.990033996748853e-2,0.6949482103298836,0.6964980225198554,7.273712500328966e-2,0.3546463262519123,0.9651352677729054,0.19215112439206283,0.1687147393374061,0.7724877740282746,0.7493500763125807,0.7465401531378396,0.13067645885983625,0.27786117158039625,0.6139241979367323,0.9290025649067571,0.6456228283378459,0.39994385679028666],[0.43148310813230495,0.8552427414350572,0.5651094447719309,1.1885230434824812e-2,0.8975769247671214,0.630475594463974,0.642694906252267,0.35988838002715606,0.18243380826581101,0.7382769270311431,0.2127522403502723,2.853536168880999e-2,0.7923542117104482,0.44261662754137976,0.9622203971633972,0.6087515798171879,0.8968952291331005,0.38232194085541304,0.542468941967437,2.2391345770331084e-2,0.6312746098222742,0.8800495690332123,0.6358527089205939,0.17540374069636833,0.5910543270584739,0.45195956326669484,0.8103201646411305,0.6593906045380183,0.2086003416180432,0.7213492725294486,0.34323395752852126,0.7776016469325404,0.45439204172260017,0.14724857637486333,0.3670599103158233,0.21863805459098562,0.5358142819492667,4.253416267540888e-2,0.10486780992623068,0.35038043230041704,0.6725286415452263,0.6919021659404545,0.6973154778641351,0.16747859328503556,0.19149508868802334,0.3974902359677992,0.9193376752067607,0.8009957360016826,4.7954066244329296e-2,0.8761375377788914,0.6504731400465841,0.18862064038111293,0.5742577598742906,0.3914156287983531,0.4990471102207531,0.6675593670617698,0.8240997060697091,0.6783574037641393,0.19001746572418876,0.5667984641081032,0.8453423404864205,0.4268092167239026,0.9738788888049947,0.18930157195785347],[0.15892588186632017,0.6275284189134824,0.9139148767980296,0.9572448010567004,0.888234680318629,0.3447132114876075,0.3191777395539295,0.6119596771995249,0.2426426357401027,0.5501526239711845,0.5255304351706805,0.8913038765940754,0.5789983242625061,3.487051386903728e-2,0.43244549547859457,0.5420623213811823,0.42004085879453623,0.5146174867478492,0.5088238961992769,0.10295308418502647,5.453302740197197e-2,0.3733408006946558,0.2366860532315408,0.8270729856272483,0.8861135045063475,0.8064788210995975,0.13847660651264593,0.8228669774938968,2.3382144421494555e-2,1.3678328502901849e-3,7.223609160819777e-2,0.6693758513271092,0.20141579207601956,8.460766106439921e-2,0.7337886758502096,0.6458067069823317,0.9854689394010867,4.498573348416868e-2,0.8300818438956467,0.8238163714030529,0.2879024368463733,0.9053904967774056,0.3906856871888179,3.7209534323734195e-2,0.23910954975264942,0.23360928168864759,0.6063620891629493,0.3003153925598009,0.833987791916494,0.9996213676376069,0.8466338781549354,0.6433223959198816,0.6212344146974014,0.21963738738910965,0.9316682477394594,0.748645012346157,0.7525059737216976,0.6156321531279068,0.9065625666775462,0.39835131629601384,0.1371492800319576,0.29664257076651723,0.7815987195896684,0.1901779138230908],[0.13848745827775177,0.9942448617979356,0.9785841416131108,7.681235232975103e-2,1.7727514064767025e-2,9.160851268321402e-2,0.6283918019787064,0.3596534943135583,0.24548389303351126,0.5715374423623141,0.15877071049845914,0.7842443590564983,0.36301709806254623,0.5069212115120356,0.30733930568055146,0.7899924826546919,0.40827910982067706,0.30270925962185213,0.8320082523438476,0.8419845409732497,0.8262749083223481,0.845218143129554,0.8257167809479945,0.23093842894468808,0.8564356445227154,0.39019956276905154,0.6787549086733672,0.2379249534197867,0.29989216265348295,0.11301559391615357,0.6018482403339829,0.36922975209493025,0.19991840467470756,8.781648793807717e-2,0.4607888368666134,0.1203211653618772,0.3691795982217163,0.7492181610638438,0.7724195400669461,4.544127573367662e-2,0.3225316104757876,0.7102961783920699,0.6755880032553977,4.833640082524504e-2,0.36320802136621866,0.5286596416235466,0.5921110580002622,0.45558000191488834,0.9754592608639556,0.9436961213820035,0.962860065394826,0.8157182782671356,0.6761264525131644,0.24134109429591932,5.934703207334413e-2,0.21192937763742514,0.8599275746934428,0.8372504223244258,0.8876400748354649,0.30853198707763596,0.7505503850104268,0.6457900885710369,0.5354846785484128,0.999595335767487],[0.37063285615956276,0.5931319322003648,0.41499270204999394,6.677596199750546e-2,0.26590529500097637,0.9988533998865615,0.16281654673535162,0.846969203288342,0.13444489835690532,0.7231200435556339,0.8694384805447563,0.49355515630163416,1.2272476645017183e-2,0.9350694073572087,0.9907932572918797,0.30257789175935945,0.48885191450694065,0.44655384798667896,0.6175026945504594,0.20084519418600444,0.4231689909292087,0.8499981937490079,0.18870276255026586,0.22969125313986705,0.43621482069691087,0.3577857375673331,0.3824440682766824,3.108959326261329e-2,5.957173073802524e-2,0.5669308515721624,0.48716306244422325,0.24632545316866516,1.8398889363218318e-2,0.994739933862064,0.24969636342016133,0.44881423695682676,0.7715616802218045,0.9415512415607005,2.2629355171546184e-3,0.34251826713153655,0.2448114921793667,8.779158199832471e-2,0.9662867482437556,0.7434114659208502,0.7174303415589914,0.9446220609661018,0.1886103216107794,0.3747898105006521,2.2914573362600166e-2,0.5318751175499192,0.7771168968969083,0.2764517531880315,0.6884711438008627,0.8244475215192086,9.387797999302017e-2,0.6969691941150492,0.47530229119159406,3.376747471776931e-2,3.315308183958243e-2,0.15477117078320957,9.415620119260348e-2,0.8802501632694097,0.7925916162676988,0.536121074640834],[0.40101483059738396,0.6662305947863542,0.6849763256389565,0.4750285988964771,0.4745031787383106,6.654062938740879e-2,0.4175600559096633,0.15601111681076796,1.6801347341492634e-2,0.21169875997634457,0.2567057854555418,0.6695059022986344,0.16533228639359498,0.6930137726146812,0.9110865204250528,0.17985705872615088,0.6035203757849298,0.30019160730714567,0.5390082020165299,0.5618063044608732,0.32974685035030593,0.15461978572436808,0.46525809799815554,0.6270141760220476,0.37808348254886504,0.18101013173060443,0.3065971401938272,0.47698279653175213,0.13182795710458373,0.49245536534167256,0.9921261730022197,0.7147706458244477,0.800101348389089,0.7759933531161417,0.5300764463663268,0.33527606943181076,0.6361586604895609,0.5508772259187362,0.29236465321544536,0.2551573913291919,0.5322446409249089,0.9000460677986389,0.21751548794345454,0.19537137836356233,0.7119742799912017,0.9830444596158785,0.9794004168584717,0.6560336466324163,0.9407337329263988,0.2944062001194274,0.6847178233754146,0.10143654112888689,0.1672934485042573,5.916625514446017e-2,0.19888173795442454,0.736655514909091,2.688208763452249e-2,0.32616869307034935,0.16300163691163227,0.5597504820155306,0.7626740548161902,0.4042225510644255,0.615384530120297,0.434335305724159],[0.10829453590620275,0.8398170261299439,0.5868121473722956,0.9163563390063925,0.19659716567306007,0.6195809184011652,0.6868365427259148,0.4046457012284558,0.9615905053138857,0.48894121692094783,0.9600749629517218,0.7522805452209659,0.4231257056951484,0.28022413587745243,0.9218945131945372,0.4546481899141932,0.6386132776817768,0.545582276470863,0.3753526791420282,0.6343968877830182,0.6357403078294218,0.72206933099147,7.822368011788994e-2,0.426779833007636,0.47836856413721784,0.19463911229929642,0.41913588735775187,0.9134844228199794,0.40589860234350483,0.9550777451855762,0.2644273543972947,0.8807407188369192,0.9812201604388174,0.11657010502205345,0.13769968795611465,0.5657244603112407,0.3767592329339152,0.9986341284303755,0.5881157179401091,0.5207376649080724,0.9933887340916198,0.27231909730225046,0.3801198248391059,0.18917024622635814,0.5334975672388201,0.7076834350026581,0.6296623219567299,0.2351453709968544,0.5738303850925909,5.950757638268167e-2,0.22698249328332076,0.5281166306615575,0.2300035919565966,0.8170908694673076,0.6782372398722614,0.531185719454117,0.9423295529781177,0.8510265628711392,0.5102392603145011,0.6540370837034533,2.002325835930896e-2,1.563773329092344e-2,0.8822872901608225,0.13817706172444322],[0.8684229268188736,0.4603246426855039,0.36958945335261095,0.8295073407570884,0.7622868427217634,0.5716030049577019,0.8531302811818371,0.4265702286184042,4.351810814350088e-2,0.12531868651625766,0.4346946867089856,6.7460642222538e-2,0.1132278472702768,0.6799107660029182,0.3852372338891664,0.8626718732411385,0.4916495483411163,0.7818899574559874,0.28149845216401803,0.8017136171172077,0.7546821830679334,0.8117444577153871,0.5916699782139158,0.5410213584521011,0.5737475658726714,9.196384008710212e-2,0.6067115761032864,0.2952894762690872,0.1637348226135732,0.9415243294590698,0.21552769722540888,0.12505593709464735,1.6034073279814987e-2,0.4025472300748759,0.8310568032625905,0.4617998774009874,0.9991876706720064,0.7376789441302188,0.32219885330767806,0.73094343426194,0.34290720246602513,0.4257360738798287,0.6252643386921937,0.5904946382737066,0.3547123532526496,0.2775818484773416,0.9211580975684901,0.31380750426011705,0.30982069418170044,0.2602552263597072,0.5692860494780222,0.3343670856126677,0.9278021367062744,0.32802405428687464,0.997305506916268,0.40622592625256193,0.5311184376250135,0.14641489317365008,0.46704137665680956,0.4863498076648216,0.7538023098981123,0.23544982755782284,0.12211960849359027,0.8520935882813153],[0.9650493160314889,0.26710293507144767,0.8122552194951567,0.6445937502095682,0.4969173638962753,0.5159761045779279,0.45014579923081166,0.9743005416578168,0.7754743169153626,0.21054408974248628,0.26258606465851664,2.6284588905803985e-2,0.4508317100678846,0.931857623852303,0.4091689797225575,0.2759604438116695,0.17694179792681541,0.4222415230106401,0.6309967577035818,5.319689396169747e-2,0.6309892549071019,0.5598094723764624,0.9087012734801726,0.17768943594837672,0.4638248519660557,0.6652881628850993,0.4018200423674927,0.7370913795637916,0.8028723534339901,0.5172331091261387,0.9838269899976873,0.9010589959251304,3.938174524073512e-2,0.19071553685530218,0.7440854920184944,0.20578621351211335,9.916543777706799e-2,0.17087289267179462,0.5727543500843529,0.34849560102413246,0.27863255509376594,0.27410338859039496,0.7862106708513065,0.637199285605627,0.7197519405107948,0.7444556831788112,0.28837010295877374,0.4627182817016181,6.564359662309394e-2,0.6186032668708884,0.7381042500932177,0.5553974148977157,0.7386642549287682,0.945302697494516,0.7840392896166968,3.0744750678418975e-2,0.4566407933976212,0.9169516003601295,0.38392775259525336,0.9102893474569151,0.11868779678588792,0.8165719506807628,0.9954945178048423,0.743361651091924],[0.3595715005920197,0.40115811199220563,0.1477850641705124,0.22472137259463532,0.23757552633171009,0.6875655884064951,0.1417792421435038,0.25001364860926534,0.6602665972827845,0.11227778236404673,0.6938484667654794,0.9062039630302379,0.37022185950455244,0.6183683139543983,0.15685240755257224,0.42234766549494074,0.3891918218441055,0.689319089121111,2.569965277163988e-2,2.076739624670032e-3,0.778704665250524,0.30038681765340647,0.2727195788548413,0.6228322110974353,0.1522201613566203,0.9388461723290575,6.263304441200723e-2,0.6885196060525295,0.4001253000373828,0.2324674269213507,0.1618692661544716,0.1877793512707362,0.31456418006566167,0.8507854568747862,0.39909529807539057,0.21663789690014867,0.4215149837882779,0.4887806847180409,0.9487442744281007,4.374725265268398e-2,0.7354504181700047,0.8997158799288029,0.23052394330356896,0.7241746615102779,0.704058334645498,4.724005810020615e-2,7.058527830001626e-2,0.8320387794834782,0.3668032712513487,0.551669542160799,0.44629289610610456,0.6643802836158343,9.470708175869813e-3,0.4654871726281721,1.2911624791987597e-2,0.6809089126513433,0.7044661907143649,0.4064463137145524,0.424811981734285,0.13700123955930332,0.10696788687261072,0.5317500983856295,0.8206126843183001,0.17960161052878587]]