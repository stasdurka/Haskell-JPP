import qualified Data.Map as Map
import Data.Maybe(fromJust)

import IntLambda
type Env = Map.Map Name Type

typeOf0 :: Env -> Exp -> Type

-- ma robic to co typeCheck z moodle'a
typeOf = typeOf0 Map.empty
