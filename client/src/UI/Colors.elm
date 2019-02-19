module UI.Colors exposing
    ( black
    , blue100
    , blue200
    , blue300
    , blue400
    , crimson100
    , crimson200
    , crimson300
    , crimson400
    , gray100
    , gray200
    , gray300
    , gray400
    , white
    )

import Css exposing (..)


white : Color
white =
    hex "#ffffff"


black : Color
black =
    hex "#000000"


gray100 : Color
gray100 =
    hex "#fdfdfd"


gray200 : Color
gray200 =
    hex "#f7f8f9"


gray300 : Color
gray300 =
    hex "#c9ccd0"


gray400 : Color
gray400 =
    hex "#a4a7ab"


blue100 : Color
blue100 =
    hex "#258bd6"


blue200 : Color
blue200 =
    hex "#1d74b4"


blue300 : Color
blue300 =
    hex "#185e92"


blue400 : Color
blue400 =
    hex "#124469"


crimson100 : Color
crimson100 =
    hex "#f03764"


crimson200 : Color
crimson200 =
    hex "#ca2f54"


crimson300 : Color
crimson300 =
    hex "#a72746"


crimson400 : Color
crimson400 =
    hex "#7d1e35"
