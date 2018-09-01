#!/bin/bash

#user=daria
#host=46.101.142.224
user=daria
host=188.246.227.172

function deploy_frontend() {
    echo -e "Deploying frontend\n"
    cd frontend
    elm test
    test_passed=`elm test | grep -c "TEST RUN PASSED"`
    if [[ $test_passed == "1" ]]
    then
        npm run build
        sed -i s'|http://localhost:8081|https://ourfamilytree.site/api|' dist/index.js
        scp -r dist $user@$host:~/fatr/frontend/
    else
        echo "Tests failed. Not deploying anything! :)"
    fi
    cd ..
}

function deploy_backend() {
    echo -e "Deploying backend\n"
    cd backend
    stack test #--coverage --ghc-options "-fforce-recomp"
    test_passed=`stack test | grep -c "0 failures"`
    if [[ $test_passed == "1" ]]
    then
        stack image container
        echo "Saving docker image as a tar. Could take a while.."
        docker save fatr-backend | gzip > fatr-backend.tar.gz
        scp fatr-backend.tar.gz $user@$host:~/fatr/
        ssh $user@$host "
            cd fatr;
            gzip -df fatr-backend.tar.gz;
            echo 'Loading docker image from tar file. Could take a while..';
            docker load -i fatr-backend.tar;
            ./restart_backend.sh;"
    else
        echo "Tests failed. Not deploying anything! =)"
    fi
    cd ..
}

function backup_database() {
    echo -e "Backup database from server on dropbox\n"
    today=`date +"%Y-%m-%d"`
    scp $user@$host:~/fatr/db/fatr.db ~/Dropbox/backup/digital_ocean/fatr_"$today".db
}

function copy_database() {
    echo -e "Copy database to server from local machine\n"
    scp backend/db/fatr.db $user@$host:~/fatr/db
}

if [[ $# -eq 0 ]] 
then
    echo "An argument --frontend or --backend or --backup-db is needed."
    exit
fi

while [[ $# > 0 ]]
do
    case "${1}" in
      -F|--frontend)
      deploy_frontend
      shift
      ;;
      -B|--backend)
      deploy_backend
      shift
      ;;
      -DB|--backup-db)
      backup_database
      shift
      ;;
      -CDB|--copy-db)
      copy_database
      shift
      ;;
      *)
      echo "${1} is not a valid flag. Only --frontend or --backend or
        --backup-db --copy-db are acceptable."
      shift
      ;;
    esac
done
