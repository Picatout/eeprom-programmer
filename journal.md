### 2025-02-20

* Agrandir le **tib** à 240 octets. Permet de revecoir des commandes **:** contenant jusqu'à 64 octets ce qui est la taille des pages des EEPROM  AT27Cxxx

### 2025-02-16
* Débuter le projet [eeProg-IDE](eeProg-IDE/eeProg_IDE.lpr), un enviromment graphique pour le programmeur eeProg conçu en free  Pascal en utilisant l'IDE [Lazarus](https://www.lazarus-ide.org/). La version 3.0 est disponible dans les dépôts de Ubuntu 24.04LTS.

* la variable *base* n'était pas initialisée.
* Modifié la routine *print_txt* pour mettre la chaîne ASCII en commentaire. 
* bogue dans XAMBLOCK
```
#0.FFFF
0000: FF    

#
```
Le problème venait de l'instruction **JRMI 2$** qui est utilisé avec les nombre signés. J'ai utilisé à la place l'instruction **JRULT**. Car il s'agit d'une comparaison non signeé entre l'adresse dans **X** et la limite **last**.

### 2025-02-12
* Modifié la routine **readln** dans [terminal.asm](terminal.asm) pour accepter des lignes de 127 caractères. De cette façon on peut programmer jusqu'à 32 octets par ligne au lieu de 16. Le fichier [wozmon.hexdump](wozmon.hexdump) a été modifié pour des lignes de 32 octets de données par ligne.
* **NOTE** le délais interligne a du être augmenté à 20 msec pour que ça fonctionne correctement.
* version 1.0R4
* en mode programmation il est maintenant possible d'ajouter un commentaire à la fin de la ligne après les données. Les commentaires débutent par un point-virgule **;**.
* version 10R5
* Modifié [readme.md](readme.md) pour indiquer qu'il est possible de programmer plusieurs lignes à la suite sans donner d'adresse pour les lignes suivant la première.
* Modifié [wozmon.hexdump](wozmon.hexdump) pour mettre l'instruction RTI à l'adresse $FFF8 et donner cette adresse pour les vecteurs IRQ et NMI.

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