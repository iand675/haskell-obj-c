{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeICECandidateStruct@.
module ObjC.Matter.MTRDataTypeICECandidateStruct
  ( MTRDataTypeICECandidateStruct
  , IsMTRDataTypeICECandidateStruct(..)
  , candidate
  , setCandidate
  , sdpMid
  , setSdpMid
  , sdpmLineIndex
  , setSdpmLineIndex
  , candidateSelector
  , sdpMidSelector
  , sdpmLineIndexSelector
  , setCandidateSelector
  , setSdpMidSelector
  , setSdpmLineIndexSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- candidate@
candidate :: IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct => mtrDataTypeICECandidateStruct -> IO (Id NSString)
candidate mtrDataTypeICECandidateStruct =
  sendMessage mtrDataTypeICECandidateStruct candidateSelector

-- | @- setCandidate:@
setCandidate :: (IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct, IsNSString value) => mtrDataTypeICECandidateStruct -> value -> IO ()
setCandidate mtrDataTypeICECandidateStruct value =
  sendMessage mtrDataTypeICECandidateStruct setCandidateSelector (toNSString value)

-- | @- sdpMid@
sdpMid :: IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct => mtrDataTypeICECandidateStruct -> IO (Id NSString)
sdpMid mtrDataTypeICECandidateStruct =
  sendMessage mtrDataTypeICECandidateStruct sdpMidSelector

-- | @- setSdpMid:@
setSdpMid :: (IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct, IsNSString value) => mtrDataTypeICECandidateStruct -> value -> IO ()
setSdpMid mtrDataTypeICECandidateStruct value =
  sendMessage mtrDataTypeICECandidateStruct setSdpMidSelector (toNSString value)

-- | @- sdpmLineIndex@
sdpmLineIndex :: IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct => mtrDataTypeICECandidateStruct -> IO (Id NSNumber)
sdpmLineIndex mtrDataTypeICECandidateStruct =
  sendMessage mtrDataTypeICECandidateStruct sdpmLineIndexSelector

-- | @- setSdpmLineIndex:@
setSdpmLineIndex :: (IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct, IsNSNumber value) => mtrDataTypeICECandidateStruct -> value -> IO ()
setSdpmLineIndex mtrDataTypeICECandidateStruct value =
  sendMessage mtrDataTypeICECandidateStruct setSdpmLineIndexSelector (toNSNumber value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @candidate@
candidateSelector :: Selector '[] (Id NSString)
candidateSelector = mkSelector "candidate"

-- | @Selector@ for @setCandidate:@
setCandidateSelector :: Selector '[Id NSString] ()
setCandidateSelector = mkSelector "setCandidate:"

-- | @Selector@ for @sdpMid@
sdpMidSelector :: Selector '[] (Id NSString)
sdpMidSelector = mkSelector "sdpMid"

-- | @Selector@ for @setSdpMid:@
setSdpMidSelector :: Selector '[Id NSString] ()
setSdpMidSelector = mkSelector "setSdpMid:"

-- | @Selector@ for @sdpmLineIndex@
sdpmLineIndexSelector :: Selector '[] (Id NSNumber)
sdpmLineIndexSelector = mkSelector "sdpmLineIndex"

-- | @Selector@ for @setSdpmLineIndex:@
setSdpmLineIndexSelector :: Selector '[Id NSNumber] ()
setSdpmLineIndexSelector = mkSelector "setSdpmLineIndex:"

