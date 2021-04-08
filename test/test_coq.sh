echo "Preparing Coq environment..."
cp ../nanoyalla/nanoll.v .
cp ../nanoyalla/macroll.v .
cp ../nanoyalla/_CoqProject .
sed -i '/example/d' _CoqProject
echo "proof_as_coq.v" >> _CoqProject
cp ../nanoyalla/configure .
./configure

echo "Executing tests..."
proof_dict='proofs/*.json'
for f in $proof_dict
do
  echo ""
  echo "$f"
  curl -s -X POST 'http://localhost:8080/export_as_coq' -H 'content-type:text/plain;charset=UTF-8' -d @$f > proof_as_coq.v
  make
done

echo "Cleaning Coq environment..."
rm nanoll.*
rm .nanoll.*
rm macroll.*
rm .macroll.*
rm _CoqProject
rm configure
rm Makefile*
rm .Makefile*
rm proof_as_coq.*
rm .proof_as_coq.*