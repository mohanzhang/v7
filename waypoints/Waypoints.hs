{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment
import Numeric

import Control.Applicative hiding (optional)
import Control.Monad (MonadPlus(..), ap)

import Data.Monoid
import Data.Maybe
import Data.List
import qualified Data.Map as M

import Text.ParserCombinators.Parsec hiding (many, (<|>))

-- runhaskell Waypoints.hs ./on_the_road_2013.txt ../partials/timeline.html

type Day = Integer
type Location = String
type Color = String
type Waypoint = (Day, Location, Maybe Color)
type Timeline = [Waypoint]

type LocationMap = M.Map Day [Location]
type DurationMap = M.Map Day Float
type ColorMap = M.Map Day Color

main = do
  args <- getArgs
  let infile:outfile:[] = args
  input <- readFile infile

  let raw = mkTimeline input
  let days = mkDays raw
  let locations = mkLocations raw
  let durations = mkDurations days
  let colors = mkColors raw

  writeFile outfile $ writeHTML days locations durations colors

  where
    mkDays :: Timeline -> [Day]
    mkDays ws = nub (map (\(d,_,_) -> d) ws)

    mkLocations :: Timeline -> LocationMap
    mkLocations ws = foldr (\(d,l,_) map ->
        M.insertWith (++) d [l] map
      ) M.empty ws

    mkDurations :: [Day] -> DurationMap
    mkDurations ds =
      let diffs = foldr (\(d1,d2) map ->
                    M.insert d1 (abs (d1-d2)) map
                  ) M.empty (zip ds (tail ds ++ [last ds]))
          total = M.foldr (+) 0 diffs
      in M.map (flip (/) (fromIntegral total) . fromIntegral) diffs

    mkColors :: Timeline -> ColorMap
    mkColors ws@((_,_,firstColor):_) = fst $
      foldr (\(d,_,c) (map,currentColor) ->
        case c of
          Nothing -> (M.insert d (fromJust currentColor) map, currentColor)
          Just newColor -> (M.insert d newColor map, c)
      ) (M.empty,firstColor) (reverse ws)

    writeHTML :: [Day] -> LocationMap -> DurationMap -> ColorMap -> String
    writeHTML ds lmap dmap cmap =
      "<table><thead><tr><th class=\"leftCol\">Day</th><th class=\"rightCol\">Location</th><tbody>"
      ++ (concat $ map (htmlWaypoint lmap dmap cmap) (reverse ds)) ++
      "</tbody></table>"

    htmlWaypoint :: LocationMap -> DurationMap -> ColorMap -> Day -> String
    htmlWaypoint lmap dmap cmap d =
      let height = (showFFloat (Just 2) (100 * (fromJust $ M.lookup d dmap))) ""
          color = fromJust $ M.lookup d cmap
          locations = fromJust $ M.lookup d lmap in
      "<tr><td class=\"leftCol\" height=\"" ++ height ++ "%\" style=\"background:" ++ color ++ ";\">"
      ++ (show d) ++ "</td><td class=\"rightCol\">" ++ (foldr (\l acc -> l ++ "<br/>" ++ acc) "" (reverse locations)) ++ "</td></tr>"

mkTimeline :: String -> Timeline
mkTimeline input = map parseWaypoint (lines input)
  where parseWaypoint line = case parse waypoint "(unknown)" line of
          Left error -> (0, show error, Nothing)
          Right output -> output

waypoint :: GenParser Char st Waypoint
waypoint = do
  d <- day
  space
  l <- location
  Text.ParserCombinators.Parsec.optional space
  c <- optionMaybe color
  return (d,l,c)

day :: GenParser Char st Day
day = do
  sign <- optionMaybe (char '-')
  num <- many digit
  return $ read (case sign of
    Nothing -> num
    Just sign -> sign:num)

location :: GenParser Char st Location
location = many (noneOf ";\n") <* string ";" <* optional space

color :: GenParser Char st Color
color = char '(' *> ((:) <$> char '#' <*> (many1 hexDigit)) <* char ')'
