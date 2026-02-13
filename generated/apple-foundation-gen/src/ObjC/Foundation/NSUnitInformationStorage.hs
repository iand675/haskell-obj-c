{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUnitInformationStorage@.
module ObjC.Foundation.NSUnitInformationStorage
  ( NSUnitInformationStorage
  , IsNSUnitInformationStorage(..)
  , bytes
  , bits
  , nibbles
  , yottabytes
  , zettabytes
  , exabytes
  , petabytes
  , terabytes
  , gigabytes
  , megabytes
  , kilobytes
  , yottabits
  , zettabits
  , exabits
  , petabits
  , terabits
  , gigabits
  , megabits
  , kilobits
  , yobibytes
  , zebibytes
  , exbibytes
  , pebibytes
  , tebibytes
  , gibibytes
  , mebibytes
  , kibibytes
  , yobibits
  , zebibits
  , exbibits
  , pebibits
  , tebibits
  , gibibits
  , mebibits
  , kibibits
  , bitsSelector
  , bytesSelector
  , exabitsSelector
  , exabytesSelector
  , exbibitsSelector
  , exbibytesSelector
  , gibibitsSelector
  , gibibytesSelector
  , gigabitsSelector
  , gigabytesSelector
  , kibibitsSelector
  , kibibytesSelector
  , kilobitsSelector
  , kilobytesSelector
  , mebibitsSelector
  , mebibytesSelector
  , megabitsSelector
  , megabytesSelector
  , nibblesSelector
  , pebibitsSelector
  , pebibytesSelector
  , petabitsSelector
  , petabytesSelector
  , tebibitsSelector
  , tebibytesSelector
  , terabitsSelector
  , terabytesSelector
  , yobibitsSelector
  , yobibytesSelector
  , yottabitsSelector
  , yottabytesSelector
  , zebibitsSelector
  , zebibytesSelector
  , zettabitsSelector
  , zettabytesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Foundation.Internal.Classes

-- | @+ bytes@
bytes :: IO (Id NSUnitInformationStorage)
bytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' bytesSelector

-- | @+ bits@
bits :: IO (Id NSUnitInformationStorage)
bits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' bitsSelector

-- | @+ nibbles@
nibbles :: IO (Id NSUnitInformationStorage)
nibbles  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' nibblesSelector

-- | @+ yottabytes@
yottabytes :: IO (Id NSUnitInformationStorage)
yottabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' yottabytesSelector

-- | @+ zettabytes@
zettabytes :: IO (Id NSUnitInformationStorage)
zettabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' zettabytesSelector

-- | @+ exabytes@
exabytes :: IO (Id NSUnitInformationStorage)
exabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' exabytesSelector

-- | @+ petabytes@
petabytes :: IO (Id NSUnitInformationStorage)
petabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' petabytesSelector

-- | @+ terabytes@
terabytes :: IO (Id NSUnitInformationStorage)
terabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' terabytesSelector

-- | @+ gigabytes@
gigabytes :: IO (Id NSUnitInformationStorage)
gigabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' gigabytesSelector

-- | @+ megabytes@
megabytes :: IO (Id NSUnitInformationStorage)
megabytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' megabytesSelector

-- | @+ kilobytes@
kilobytes :: IO (Id NSUnitInformationStorage)
kilobytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' kilobytesSelector

-- | @+ yottabits@
yottabits :: IO (Id NSUnitInformationStorage)
yottabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' yottabitsSelector

-- | @+ zettabits@
zettabits :: IO (Id NSUnitInformationStorage)
zettabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' zettabitsSelector

-- | @+ exabits@
exabits :: IO (Id NSUnitInformationStorage)
exabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' exabitsSelector

-- | @+ petabits@
petabits :: IO (Id NSUnitInformationStorage)
petabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' petabitsSelector

-- | @+ terabits@
terabits :: IO (Id NSUnitInformationStorage)
terabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' terabitsSelector

-- | @+ gigabits@
gigabits :: IO (Id NSUnitInformationStorage)
gigabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' gigabitsSelector

-- | @+ megabits@
megabits :: IO (Id NSUnitInformationStorage)
megabits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' megabitsSelector

-- | @+ kilobits@
kilobits :: IO (Id NSUnitInformationStorage)
kilobits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' kilobitsSelector

-- | @+ yobibytes@
yobibytes :: IO (Id NSUnitInformationStorage)
yobibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' yobibytesSelector

-- | @+ zebibytes@
zebibytes :: IO (Id NSUnitInformationStorage)
zebibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' zebibytesSelector

-- | @+ exbibytes@
exbibytes :: IO (Id NSUnitInformationStorage)
exbibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' exbibytesSelector

-- | @+ pebibytes@
pebibytes :: IO (Id NSUnitInformationStorage)
pebibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' pebibytesSelector

-- | @+ tebibytes@
tebibytes :: IO (Id NSUnitInformationStorage)
tebibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' tebibytesSelector

-- | @+ gibibytes@
gibibytes :: IO (Id NSUnitInformationStorage)
gibibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' gibibytesSelector

-- | @+ mebibytes@
mebibytes :: IO (Id NSUnitInformationStorage)
mebibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' mebibytesSelector

-- | @+ kibibytes@
kibibytes :: IO (Id NSUnitInformationStorage)
kibibytes  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' kibibytesSelector

-- | @+ yobibits@
yobibits :: IO (Id NSUnitInformationStorage)
yobibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' yobibitsSelector

-- | @+ zebibits@
zebibits :: IO (Id NSUnitInformationStorage)
zebibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' zebibitsSelector

-- | @+ exbibits@
exbibits :: IO (Id NSUnitInformationStorage)
exbibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' exbibitsSelector

-- | @+ pebibits@
pebibits :: IO (Id NSUnitInformationStorage)
pebibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' pebibitsSelector

-- | @+ tebibits@
tebibits :: IO (Id NSUnitInformationStorage)
tebibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' tebibitsSelector

-- | @+ gibibits@
gibibits :: IO (Id NSUnitInformationStorage)
gibibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' gibibitsSelector

-- | @+ mebibits@
mebibits :: IO (Id NSUnitInformationStorage)
mebibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' mebibitsSelector

-- | @+ kibibits@
kibibits :: IO (Id NSUnitInformationStorage)
kibibits  =
  do
    cls' <- getRequiredClass "NSUnitInformationStorage"
    sendClassMessage cls' kibibitsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @bytes@
bytesSelector :: Selector '[] (Id NSUnitInformationStorage)
bytesSelector = mkSelector "bytes"

-- | @Selector@ for @bits@
bitsSelector :: Selector '[] (Id NSUnitInformationStorage)
bitsSelector = mkSelector "bits"

-- | @Selector@ for @nibbles@
nibblesSelector :: Selector '[] (Id NSUnitInformationStorage)
nibblesSelector = mkSelector "nibbles"

-- | @Selector@ for @yottabytes@
yottabytesSelector :: Selector '[] (Id NSUnitInformationStorage)
yottabytesSelector = mkSelector "yottabytes"

-- | @Selector@ for @zettabytes@
zettabytesSelector :: Selector '[] (Id NSUnitInformationStorage)
zettabytesSelector = mkSelector "zettabytes"

-- | @Selector@ for @exabytes@
exabytesSelector :: Selector '[] (Id NSUnitInformationStorage)
exabytesSelector = mkSelector "exabytes"

-- | @Selector@ for @petabytes@
petabytesSelector :: Selector '[] (Id NSUnitInformationStorage)
petabytesSelector = mkSelector "petabytes"

-- | @Selector@ for @terabytes@
terabytesSelector :: Selector '[] (Id NSUnitInformationStorage)
terabytesSelector = mkSelector "terabytes"

-- | @Selector@ for @gigabytes@
gigabytesSelector :: Selector '[] (Id NSUnitInformationStorage)
gigabytesSelector = mkSelector "gigabytes"

-- | @Selector@ for @megabytes@
megabytesSelector :: Selector '[] (Id NSUnitInformationStorage)
megabytesSelector = mkSelector "megabytes"

-- | @Selector@ for @kilobytes@
kilobytesSelector :: Selector '[] (Id NSUnitInformationStorage)
kilobytesSelector = mkSelector "kilobytes"

-- | @Selector@ for @yottabits@
yottabitsSelector :: Selector '[] (Id NSUnitInformationStorage)
yottabitsSelector = mkSelector "yottabits"

-- | @Selector@ for @zettabits@
zettabitsSelector :: Selector '[] (Id NSUnitInformationStorage)
zettabitsSelector = mkSelector "zettabits"

-- | @Selector@ for @exabits@
exabitsSelector :: Selector '[] (Id NSUnitInformationStorage)
exabitsSelector = mkSelector "exabits"

-- | @Selector@ for @petabits@
petabitsSelector :: Selector '[] (Id NSUnitInformationStorage)
petabitsSelector = mkSelector "petabits"

-- | @Selector@ for @terabits@
terabitsSelector :: Selector '[] (Id NSUnitInformationStorage)
terabitsSelector = mkSelector "terabits"

-- | @Selector@ for @gigabits@
gigabitsSelector :: Selector '[] (Id NSUnitInformationStorage)
gigabitsSelector = mkSelector "gigabits"

-- | @Selector@ for @megabits@
megabitsSelector :: Selector '[] (Id NSUnitInformationStorage)
megabitsSelector = mkSelector "megabits"

-- | @Selector@ for @kilobits@
kilobitsSelector :: Selector '[] (Id NSUnitInformationStorage)
kilobitsSelector = mkSelector "kilobits"

-- | @Selector@ for @yobibytes@
yobibytesSelector :: Selector '[] (Id NSUnitInformationStorage)
yobibytesSelector = mkSelector "yobibytes"

-- | @Selector@ for @zebibytes@
zebibytesSelector :: Selector '[] (Id NSUnitInformationStorage)
zebibytesSelector = mkSelector "zebibytes"

-- | @Selector@ for @exbibytes@
exbibytesSelector :: Selector '[] (Id NSUnitInformationStorage)
exbibytesSelector = mkSelector "exbibytes"

-- | @Selector@ for @pebibytes@
pebibytesSelector :: Selector '[] (Id NSUnitInformationStorage)
pebibytesSelector = mkSelector "pebibytes"

-- | @Selector@ for @tebibytes@
tebibytesSelector :: Selector '[] (Id NSUnitInformationStorage)
tebibytesSelector = mkSelector "tebibytes"

-- | @Selector@ for @gibibytes@
gibibytesSelector :: Selector '[] (Id NSUnitInformationStorage)
gibibytesSelector = mkSelector "gibibytes"

-- | @Selector@ for @mebibytes@
mebibytesSelector :: Selector '[] (Id NSUnitInformationStorage)
mebibytesSelector = mkSelector "mebibytes"

-- | @Selector@ for @kibibytes@
kibibytesSelector :: Selector '[] (Id NSUnitInformationStorage)
kibibytesSelector = mkSelector "kibibytes"

-- | @Selector@ for @yobibits@
yobibitsSelector :: Selector '[] (Id NSUnitInformationStorage)
yobibitsSelector = mkSelector "yobibits"

-- | @Selector@ for @zebibits@
zebibitsSelector :: Selector '[] (Id NSUnitInformationStorage)
zebibitsSelector = mkSelector "zebibits"

-- | @Selector@ for @exbibits@
exbibitsSelector :: Selector '[] (Id NSUnitInformationStorage)
exbibitsSelector = mkSelector "exbibits"

-- | @Selector@ for @pebibits@
pebibitsSelector :: Selector '[] (Id NSUnitInformationStorage)
pebibitsSelector = mkSelector "pebibits"

-- | @Selector@ for @tebibits@
tebibitsSelector :: Selector '[] (Id NSUnitInformationStorage)
tebibitsSelector = mkSelector "tebibits"

-- | @Selector@ for @gibibits@
gibibitsSelector :: Selector '[] (Id NSUnitInformationStorage)
gibibitsSelector = mkSelector "gibibits"

-- | @Selector@ for @mebibits@
mebibitsSelector :: Selector '[] (Id NSUnitInformationStorage)
mebibitsSelector = mkSelector "mebibits"

-- | @Selector@ for @kibibits@
kibibitsSelector :: Selector '[] (Id NSUnitInformationStorage)
kibibitsSelector = mkSelector "kibibits"

