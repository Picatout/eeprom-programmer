### 2025-02-11 
* Correction d'un bogue dans routine **print_mem**. 
* version 1.0R1
* Corrigé bogue dans macro **_prog_delay** et dans routine **Timer4UpdateHandler*. 
* version 1.0R2 
* Ajout du fichier [wozmon.hexdump](wozmon.hexdump) au git.
* version 1.0r3

### 2025-02-03
* Création du dépôt sur [github](https://github.com/Picatout/eeprom-programmer).

### 2025-02-02
* Le travail avance sur [eeProg.asm](eeProg.asm).
* Fonction lecture et programmation de l'EEPROM testés et fonctionnels.
* Ajout des commandes 
    * __adr"STRING__  pour programmer une chaîne de caracctère dans l'EEPROM.
    * __adr1Xadr2__  pour effacer un intervalle de mémoire dans l'EEPROM en écrivant la valeur __0xFF__.
#### session 2
* Modifié routine **exam_block** pour afficher les caractères ASCII à la fin de chaque ligne d'octets.

### 2025-02-01
* Assemblage du circuit. 
* Vérification du montage.

### 2025-01-31
* création du projet.
* création de la schématique du programmeur dans KiCAD.
* Le travail avance sur [eeProg.asm](eeProg.asm).