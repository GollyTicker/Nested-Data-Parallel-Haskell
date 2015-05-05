

NAME="ListDotP"
SUF=".hs"
INFIX=".simpl"


if [ -z $2 ]; then
    A=""
else
    NAME=$2
fi

ghc -c \
     -ddump-simpl -ddump-cmm \
     -dsuppress-idinfo \
     -dsuppress-coercions -dsuppress-type-applications \
     -dsuppress-uniques -dsuppress-module-prefixes \
     "$1" -O4 "$NAME$SUF" > "$NAME$INFIX$SUF"

