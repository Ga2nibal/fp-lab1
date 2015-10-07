import System.Environment

main::IO()
main = getArgs >>= print . haqify . head

haqify s = "Haqfad! " ++ s