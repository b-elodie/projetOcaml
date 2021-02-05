#!/bin/bash

oldIFS=$IFS
IFS=$'\n'



liste_fichiers=`ls tests/ -I tests_explications.txt`
mapfile -t liste_noms < tests/tests_explications.txt


let "index = 0"


echo "Lancement des tests sans l'option paf."
echo ""
for fichier in $liste_fichiers
do
        
        echo ${liste_noms[$index]}
        echo "Voici le fichier de test :"
        cat tests/$fichier
        echo ""
        echo "Voici le résultat de l'exécution du programme sur ce fichier de test :"
        ./main.native < tests/$fichier
        echo ""


        let "index += 1"
done


let "index = 0"
echo ""
echo ""
echo ""
echo ""
echo "Lancement des tests avec l'option paf (paf !)."
echo ""


for fichier in $liste_fichiers
do
        echo ${liste_noms[$index]}
        echo "Voici le fichier de test :"
        cat tests/$fichier
        echo ""
        echo "Voici le résultat de l'exécution du programme sur ce fichier de test, avec l'option paf :"
        ./main.native -paf < tests/$fichier
        echo ""


        let "index += 1"
done

#pair