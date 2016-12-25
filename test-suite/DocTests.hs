import Test.DocTest

main::IO ()
main = doctest ["-isrc", "src/Blockchain/Ethereum/RLP.hs"]
