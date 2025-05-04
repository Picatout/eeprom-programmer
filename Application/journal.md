# Journal du projet **eeProg-GUI**

### 2025-05-04
* Version 2.1.0 de l'application. Adaptation pour la mouvelle commande 0|1V du firwmare.

### 2025-04-25

* Version 2.0.6 de l'application. Corrigé bogue dans la programmation des fichiers binaires.

### 2025-03-31

* GUI_V2.0.R5, Correction mineures
  * le contrôle EditCmd cachait le contrôle LblCmd.
  * Renommé l'exécutable simplement eeProg au lieu de eeProg-gui.

### 2025-03-30

* GUI_V2.0R4   Le problème de perte de données semble réglé.

### 2025-03-27

* Version GUI_V2.0R2 complétée.
    * Correction d'un bogue dans la routine ParseLine, le charactère **;** était sauté et l'analyse continuait dans le commentaire provoquant un over run du buffer. 

### 2025-03-26

* Débuté le travail sur la version GUI_V2.0R0 

### 2025-03-02
* Version GUI V1.0R2 
* Modifié le style de la fenêtre principale.
* Modififé l'action de certain dialogues.
* Version GUI V1.0R3. Modification du dialogue About et du dialogue **Serial port Config** pour que le premier item de la liste soit sélectionné automatiquement.

### 2025-02-20

* Complété la fonction **dump as binary file**.

* Complété la fonction **Prog binary file**.

* Complété tous les menus. Version 1.0R0

* Correction d'un bogue dans la fonction **dump** enn format hexadecimal, la première ligne n'était pas enregistrée dans le fichier.

### 2025-02-19

* Réorganisation du menu.
* Création du dialogue **RANGE**.
* Développer la fonction du menu **view range**
* Création du dialogue EEPROM
* Ajout du menu **eeprom** au menu **config**
* Modifié le Range dialog pour ajouter l'option type de fichier en sortie pour la fonction Dump.

### 2025-02-18

* Travail sur menu Send Hex file.

### 2025-02-17

* Travail sur dialgue PortCfg.

### 2025-02-16
* Début du projet dans Lazarus.
