echo "Preparing Coq environment..."

cp ./nanoyalla/nanoll.v .
coqc -R . NanoYalla nanoll.v
cp ./nanoyalla/macroll.v .
coqc -R . NanoYalla macroll.v

cp ./test/coq_test_data/example1.v .
coqc -R . NanoYalla example1.v
cp ./test/coq_test_data/example2.v .
coqc -R . NanoYalla example2.v
cp ./test/coq_test_data/example3.v .
coqc -R . NanoYalla example3.v

echo "Executing tests..."

proofs_directory='test/coq_test_data/*.json'
for f in $proofs_directory
do
  echo "$f"
  curl -s -X POST 'http://localhost:8080/export_as_coq' -H 'content-type:text/plain;charset=UTF-8' -d @"$f" > proof_as_coq.v
  coqc -R . NanoYalla proof_as_coq.v || cat proof_as_coq.v
done

echo "Cleaning Coq environment..."
rm nanoll.*
rm .nanoll.*
rm macroll.*
rm .macroll.*
rm example*.*
rm .example*.*
rm proof_as_coq.*
rm .proof_as_coq.*
