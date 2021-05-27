echo "Preparing Coq environment..."

cp ./nanoyalla/nanoll.v .
coqc -R . NanoYalla nanoll.v
cp ./nanoyalla/macroll.v .
coqc -R . NanoYalla macroll.v
if [ "$1" = "yalla" ]
then
    cp ./nanoyalla/yallacut.v nanollcut.v
else
    cp ./nanoyalla/axiomcut.v nanollcut.v
fi;
coqc -R . NanoYalla nanollcut.v
cp ./nanoyalla/macrollcut.v .
coqc -R . NanoYalla macrollcut.v

echo "Executing Coq tests..."

coq_directory='test/coq_test_data/*.v'
for f in $coq_directory
do
  echo "$f"
  cp -f "$f" ccLLproof.v
  coqc -R . NanoYalla ccLLproof.v || cat ccLLproof.v
done

echo "Executing generic tests..."

proofs_directory='test/proof_test_data/*.json'
for f in $proofs_directory
do
  echo "$f"
  curl -s -X POST 'http://localhost:8080/export_as_coq' -H 'content-type:text/plain;charset=UTF-8' -d @"$f" > ccLLproof.v
  coqc -R . NanoYalla ccLLproof.v || cat ccLLproof.v
done

echo "Cleaning Coq environment..."
rm nanoll.*
rm .nanoll.*
rm macroll.*
rm .macroll.*
rm nanollcut.*
rm .nanollcut.*
rm macrollcut.*
rm .macrollcut.*
rm ccLLproof.*
rm .ccLLproof.*
