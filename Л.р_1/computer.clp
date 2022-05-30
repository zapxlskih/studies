(deffunction ask-question (?question $?allowed-values)
   (print ?question)
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (print ?question)
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer)

(deffunction yes-or-no-p (?question)
   (bind ?response (ask-question ?question yes no y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no))


;;;***************
;;;* QUERY RULES *
;;;***************

(defrule determine-pc_starts-state ""
   (not (pc_starts ?))
   (not (repair ?))
   =>
   (assert(pc_starts (yes-or-no-p "Компьютер включается? (yes/no)? "))))

(defrule determine-power_connection-state ""
   (pc_starts no)
   (not (repair ?))   
   =>
   (assert (power_connection (yes-or-no-p "Провод питания подключен? (yes/no)? "))))

(defrule determine-power_supply_working-state ""
   (pc_starts no)
   (power_connection yes)
   (not (repair ?))
   =>
   (assert(power_supply_working (yes-or-no-p "Блок питания работает? (yes/no)? "))))

(defrule determine-bios_battery_light-state ""
   (pc_starts no)
   (power_connection yes)
   (power_supply_working yes)
   (not (repair ?))
   =>
   (assert (bios_battery_light (yes-or-no-p "Горит ли индикатор батареи BIOS? (yes/no)? "))))

(defrule determine-motherboard_working-state ""
   (pc_starts no)
   (power_connection yes)
   (power_supply_working yes)
   (bios_battery_light yes)
   (not (repair ?))
   =>
   (assert(motherboard_working (yes-or-no-p "Материнская плата работает? (yes/no)? "))))

(defrule determine-after_disconnect_parts-state ""
   (pc_starts no)
   (power_connection yes)
   (power_supply_working yes)
   (bios_battery_light yes)
   (motherboard_working yes)
   (not (repair ?))
   =>
   (assert(after_disconnect_parts (ask-question "Попробуйте отключить различные периферийные устройства. После отключения какой части ПК работает? (disk/CPU/GPU/RAM) " disk gpu cpu ram))))

(defrule determine-os_starts-state ""
   (pc_starts yes)
   (not (repair ?))
   =>
   (assert(os_starts (yes-or-no-p "Операционная система загружается? (yes/no)? "))))

(defrule determine-repair_mode-state ""
   (pc_starts yes)
   (os_starts yes)
   (not (repair ?))
   =>
   (assert(repair_mode (yes-or-no-p "Запускается ли режим восстановления ОС (yes/no)? "))))

(defrule determine-automatic_repair-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode yes)
   (not (repair ?))
   =>
   (assert(automatic_repair (yes-or-no-p "Автоматический ремонт работает? (yes/no)? "))))

(defrule determine-reset_updates-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode yes)
   (automatic_repair no)
   (not (repair ?))
   =>
   (assert(reset_updates (yes-or-no-p "Можно ли сбросить последние обновления (yes/no)? "))))

(defrule determine-repair_cmd_access-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode yes)
   (automatic_repair no)
   (reset_updates no)
   (not (repair ?))
   =>
   (assert(repair_cmd_access (yes-or-no-p "Можете ли вы получить доступ к командной строке восстановления (yes/no)? "))))

(defrule determine-bios_access-state ""
   (pc_starts yes)
   (os_starts no)
   (not (repair ?))
   =>
   (assert(bios_access (yes-or-no-p "Можете ли вы зайти в BIOS (yes/no)? "))))

(defrule determine-bios_uptodate-state ""
   (pc_starts yes)
   (os_starts no)
   (bios_access yes)
   (not (repair ?))
   =>
   (assert(bios_uptodate (yes-or-no-p "Версия BIOS актуальна? (yes/no)? "))))

(defrule determine-boot_order-state ""
   (pc_starts yes)
   (os_starts no)
   (bios_access yes)
   (bios_uptodate yes)
   (not (repair ?))
   =>
   (assert(boot_order (ask-question "Какой элемент стоит первым в порядке загрузки? (CD/network/USB/disk/other)? " cd network usb disk other))))

(defrule determine-usb_adapter_access-state ""
   (or   (and  (pc_starts yes)
               (os_starts no)
               (bios_access yes)
               (bios_uptodate yes)
               (boot_order disk))
         (and  (pc_starts yes)
               (os_starts yes)
               (repair_mode yes)
               (automatic_repair no)
               (reset_updates no)
               (repair_cmd_access no))
         (and  (pc_starts yes)
               (os_starts yes)
               (repair_mode no)
               (login_screen no))
         (and  (pc_starts no)
               (power_connection yes)
               (power_supply_working yes)
               (bios_battery_light yes)
               (motherboard_working yes)
               (after_disconnect_parts disk)))
   (not (repair ?))
   =>
   (assert(usb_adapter_access (yes-or-no-p "Можете ли вы подключить диск через USB (yes/no)? "))))

(defrule determine-chkdsk-state ""
   (or   (and  (pc_starts yes)
               (os_starts no)
               (bios_access yes)
               (bios_uptodate yes)
               (boot_order disk)
               (usb_adapter_access yes))
         (and  (pc_starts yes)
               (os_starts yes)
               (repair_mode yes)
               (automatic_repair no)
               (reset_updates no)
               (repair_cmd_access yes)))
   (not (repair ?))
   =>
   (assert(chkdsk (yes-or-no-p "Запустите chkdsk /r /f /x Ваш ПК заработал? (yes/no)? "))))

(defrule determine-login_screen-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode no)
   (not (repair ?))
   =>
   (assert(login_screen (yes-or-no-p "Появляется ли экран входа? (yes/no)? "))))

(defrule determine-login_possible-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode no)
   (login_screen yes)
   (not (repair ?))
   =>
   (assert(login_possible (yes-or-no-p "Можете ли вы зайти? (yes/no)? "))))

(defrule determine-company_network-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode no)
   (login_screen yes)
   (login_possible no)
   (not (repair ?))
   =>
   (assert(company_network (yes-or-no-p "Подключен ли ваш ПК к сети? (yes/no)? "))))

(defrule determine-local_administrator-state ""
   (pc_starts yes)
   (os_starts yes)
   (repair_mode no)
   (login_screen yes)
   (login_possible no)
   (company_network no)
   (not (repair ?))
   =>
   (assert(local_administrator (yes-or-no-p "Можно ли подключиться через учетную запись администратора (yes/no)? "))))

;;;****************
;;;* REPAIR RULES *
;;;****************

(defrule no_power_connection ""
   (power_connection no)
   (not (repair ?))
   =>
   (assert (repair "Подключите питание.")))

(defrule power_supply_defect ""
   (power_supply_working no)
   (not (repair ?))
   =>
   (assert (repair "Замените блок питания.")))

(defrule no_bios_battery_light ""
   (bios_battery_light no)
   (not (repair ?))
   =>
   (assert (repair "Замените батарейку BIOS.")))

(defrule motherboard_defect ""
   (motherboard_working no)
   (not (repair ?))
   =>
   (assert (repair "Замените материнскую плату.")))

(defrule part_defect_gpu ""
   (after_disconnect_parts gpu)
   (not (repair ?))
   =>
   (assert (repair "Замените видеокарту.")))

(defrule part_defect_cpu ""
   (after_disconnect_parts cpu)
   (not (repair ?))
   =>
   (assert (repair "Замените процессор.")))

(defrule part_defect_ram ""
   (after_disconnect_parts ram)
   (not (repair ?))
   =>
   (assert (repair "Проверьте оперативную память.")))

(defrule no_bios_access ""
   (bios_access no)
   (not (repair ?))
   =>
   (assert (repair "Прошить BIOS")))

(defrule bios_is_not_uptodate ""
   (bios_uptodate no)
   (not (repair ?))
   =>
   (assert (repair "Обновить BIOS")))

(defrule boot_order_false ""
   (or   (boot_order cd)
         (boot_order network)
         (boot_order usb)
         (boot_order other))
   (not (repair ?))
   =>
   (assert (repair "Изменить приоритет загрузочного диска.")))

(defrule disk_defective ""
   (usb_adapter_access no)
   (not (repair ?))
   =>
   (assert (repair "Диск неисправен.")))

(defrule chkdsk_not_working ""
   (chkdsk no)
   (not (repair ?))
   =>
   (assert (repair "Сохраните данные, переустановите ОС на новый диск")))

(defrule reset_updates_works ""
   (reset_updates yes)
   (not (repair ?))
   =>
   (assert (repair "Перезапустите и попробуйте обновить снова.")))

(defrule connection_with_company ""
   (company_network yes)
   (not (repair ?))
   =>
   (assert (repair "Разрешено ли изменять пароль через Active Directory.")))

(defrule local_administrator_works ""
   (local_administrator yes)
   (not (repair ?))
   =>
   (assert (repair "Откройте браузер, измените пароль, выберите пользователя и войдите в систему.")))

(defrule local_administrator_not_working ""
   (local_administrator no)
   (not (repair ?))
   =>
   (assert (repair "Отнесите ПК в сервис")))

(defrule repair_done ""
   (or   (chkdsk yes)
         (automatic_repair yes)
         (login_possible yes))
   (not (repair ?))
   =>
   (assert (repair "Ремонт закончен")))

;;;********************************
;;;* STARTUP AND CONCLUSION RULES *
;;;********************************

(defrule system-banner ""
  (declare (salience 10))
  =>
  (println crlf "Диагностика неполадок ПК" crlf))

(defrule print-repair ""
  (declare (salience 10))
  (repair ?item)
  =>
  (println crlf "Вариант ремонта:" crlf)
  (println " " ?item crlf))