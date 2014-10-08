import Numeric

data RGBColor = RGBColor(Int, Int, Int) deriving (Show, Eq)
data HSVColor = HSVColor(Int, Int, Int) deriving (Show, Eq)

-- Hex Functions

validHexString :: String
validHexString = "0123456789ABCDEFabcdef"

validHexColor :: String -> Bool
validHexColor [] = False
validHexColor (x:xs) = (x == '#') && (length $ filter (`elem` validHexString) xs) == 6	

intToHex :: Int -> String
intToHex x = showHex x ""

hexToInt :: String -> Int
hexToInt []  = 0
hexToInt str = fst . head $ readHex str

-- List Functions

group :: Int -> [a] -> [[a]]
group _ []    = []
group n l
  | n > 0     = (take n l) : (group n (drop n l))
  | otherwise = group (abs n) l

listToTuple :: [a] -> (a, a, a)
listToTuple [x,y,z] = (x,y,z)
listToTuple _       = error "Not enough params for listToTuple"

mapTuple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple f (a,b,c) = (f a,f b,f c)


-- RGBColor Conversion Functions

rgbColorintToHexColor :: RGBColor -> String
rgbColorintToHexColor (RGBColor (r, g, b)) = "#" ++ (intToHex r) ++ (intToHex g) ++ (intToHex b)

hexColorToRGBColor :: String -> RGBColor
hexColorToRGBColor (x:xs)
	| validHexColor (x:xs) = RGBColor (hexStringToTuple xs)
	| otherwise            = RGBColor(0,0,0)

hexStringToTuple :: String -> (Int, Int, Int)
hexStringToTuple xs = listToTuple $ map hexToInt (group 2 xs)


rgbColorToHSVColor :: RGBColor -> HSVColor
rgbColorToHSVColor (RGBColor (r,g,b)) | mx == mn  = HSVColor(0,0,truncate mx)
                 					  | otherwise = HSVColor fin
 									where
  										mR            = toFrac r
  										mG            = toFrac g
  										mB            = toFrac b
  										toFrac x      = fromIntegral(x)/255
  										mx            = maximum [mR,mG,mB]
  										mn            = minimum [mR,mG,mB]
  										d | mR == mn  = mG - mB
  										  | mB == mn  = mR - mG
  										  | otherwise = mB - mR
  										h | mR == mn  = 3
  										  | mB == mn  = 1
  										  | otherwise = 5
  										cmpH          = 60 * (h - d / (mx - mn))
  										cmpS          = ((mx - mn)/mx) * 100
  										cmpV          = mx * 100
  										fin           = mapTuple truncate (cmpH,cmpS,cmpV) 


--HSVColor Conversion Functions

hsvColorToRGBColor :: HSVColor -> RGBColor
hsvColorToRGBColor (HSVColor (hue, sat, val)) =
	case h of
		0 -> convertHSVMod mVal t p
		1 -> convertHSVMod q mVal p
		2 -> convertHSVMod p mVal t
		3 -> convertHSVMod p q mVal
		4 -> convertHSVMod t p mVal
		5 -> convertHSVMod mVal p q
		_ -> error "Undefined modified hue value"
		where
			mHue = (fromIntegral hue) / 360 
			mSat = (fromIntegral sat) / 100
			mVal = (fromIntegral val) / 100
			h    = truncate (mHue * 6)
			f    = mHue * 6 - (fromIntegral h)
			p    = mVal * (1 - mSat)
			q    = mVal * (1 - f * mSat)
			t    = mVal * (1 - (1 - f) * mSat)

convertHSVMod :: Float -> Float -> Float -> RGBColor
convertHSVMod r g b = RGBColor(mR,mG,mB)
					where
						mR     = conv r  
						mG     = conv g  
						mB     = conv b 
						conv x = fromIntegral $ truncate (x * 255)  