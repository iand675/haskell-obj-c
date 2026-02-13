{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Represents single interface-bytes group of ATR.
--
-- Generated bindings for @TKSmartCardATRInterfaceGroup@.
module ObjC.CryptoTokenKit.TKSmartCardATRInterfaceGroup
  ( TKSmartCardATRInterfaceGroup
  , IsTKSmartCardATRInterfaceGroup(..)
  , ta
  , tb
  , tc
  , protocol
  , protocolSelector
  , taSelector
  , tbSelector
  , tcSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CryptoTokenKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | TA interface byte of ATR group, or nil if TA is not present.
--
-- ObjC selector: @- TA@
ta :: IsTKSmartCardATRInterfaceGroup tkSmartCardATRInterfaceGroup => tkSmartCardATRInterfaceGroup -> IO (Id NSNumber)
ta tkSmartCardATRInterfaceGroup =
  sendMessage tkSmartCardATRInterfaceGroup taSelector

-- | TB interface byte of ATR group, or nil if TB is not present.
--
-- ObjC selector: @- TB@
tb :: IsTKSmartCardATRInterfaceGroup tkSmartCardATRInterfaceGroup => tkSmartCardATRInterfaceGroup -> IO (Id NSNumber)
tb tkSmartCardATRInterfaceGroup =
  sendMessage tkSmartCardATRInterfaceGroup tbSelector

-- | TC interface byte of ATR group, or nil if TC is not present.
--
-- ObjC selector: @- TC@
tc :: IsTKSmartCardATRInterfaceGroup tkSmartCardATRInterfaceGroup => tkSmartCardATRInterfaceGroup -> IO (Id NSNumber)
tc tkSmartCardATRInterfaceGroup =
  sendMessage tkSmartCardATRInterfaceGroup tcSelector

-- | Protocol number for this group.  First group (global) has protocol unassigned, contains nil.
--
-- ObjC selector: @- protocol@
protocol :: IsTKSmartCardATRInterfaceGroup tkSmartCardATRInterfaceGroup => tkSmartCardATRInterfaceGroup -> IO (Id NSNumber)
protocol tkSmartCardATRInterfaceGroup =
  sendMessage tkSmartCardATRInterfaceGroup protocolSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @TA@
taSelector :: Selector '[] (Id NSNumber)
taSelector = mkSelector "TA"

-- | @Selector@ for @TB@
tbSelector :: Selector '[] (Id NSNumber)
tbSelector = mkSelector "TB"

-- | @Selector@ for @TC@
tcSelector :: Selector '[] (Id NSNumber)
tcSelector = mkSelector "TC"

-- | @Selector@ for @protocol@
protocolSelector :: Selector '[] (Id NSNumber)
protocolSelector = mkSelector "protocol"

