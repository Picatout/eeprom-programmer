# eeProg-gui

Il s'agit d'un environnement avec interface utilisateur graphique pour le programmeur **eeProg**. Ce projet est réalisé en Free Pascal en utilisant l'environnement [Lazarus 3.0](https://www.lazarus-ide.org/) disponible dans les dépôts d'Ubuntu 24.04LTS.

Cet environnement offre des facilités supplémentaires. Par rapport à la ligne de commande utilisée dans un terminal.

1.  Possibilité de programmer une eeprom directement avec le fichier binaire brute généré par un compilateur ou assembleur.

1.  Possibilité de programmer une EEPROM en utilisant le format **HEXDUMP** déjà compatible avec le firmware de eeProg.

1. Télécharger le contenue d'une EEPROM dans un fichier au format **binaire brute** ou au format **HEXDUMP**.

### 2025-02-20

La version 1.0R0 est complétée.

### vidéo de démonstration

J'ai créé un vidéo de démonstration du programme sur Youtube.

[démontration de eeProg-gui](https://youtu.be/ZQuFBCbd9YM?si=Wmx8NIaezMBcxAin)

### Version 2.2 du programmeur
J'ai créé une version amélioré du programmeur. Cette version permet ajoute une embase **PLCC-32** pour permettre de programmer les EEPROM qui sont dans ce format. On peut programmer les EEPROM fonctinnant soit à 5 volts ou bien à 3.3 volts grâce au jumper **JP3** de la carte **NUCLEO-8S208RB** qui permet de sélectionner l'alimentation du MCU entre 3.3 volts et 5 volts. 

Les EEPROMS de type *SST39SF010A (128KO)/ SST39SF020A (256KO) / SST39SF040 (512KO)** de Microchip disponible au format **PLCC-32** pourront être programmées lorsque j'aurai modifié le logiciel, ce sera la prochaine étape.
  