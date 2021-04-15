echo "Executing generic tests..."

proofs_directory='test/proof_test_data/*.json'
for f in $proofs_directory
do
  echo "$f"
  response=$(curl -s -X POST --write-out '%{http_code}' --output /dev/null 'http://localhost:8080/export_as_latex?format=pdf' -H 'content-type:text/plain;charset=UTF-8' -d @"$f")
  if [[ $response -ne 200 ]] ; then
    echo -e "\e[01;31mFailure...\e[0m"
    exit 1
  fi
done

echo "Success!"