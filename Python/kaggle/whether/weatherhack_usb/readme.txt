hours_since, sq_time, precipitation, rain — только в тренировочном наборе
precipitation — осадки, мм/ч
rain = precipitation > 0.25 — таргет
sq_time — время измерения (timestamp, UTC)
hours_since — число часов с полуночи 1 июля по Москве.

sq_lat — широта центра квадрата, градусы
sq_lon — долгота центра квадрата, градусы
sq_x — абсцисса квадрата на сетке агрегации, целая
sq_y — ордината квадрата на сетке агрегации, целая
day_hour = hours_since % 24
city_code — город (16, 77 или 78)
hour_hash — хэш часа измерения в конкретном городе

u_hashed — обфусцированный DeviceID (в тесте постоянен в пределах часа)
ver_hash — обфусцированная версия Андроида
device_model_hash — обфусцированная модель телефона
ulat — широта пользователя
ulon — долгота пользователя 

EventTimestampDelta — время до конца часа, с
SignalStrength — сила сигнала, дБ
cell_hash —  обфусцированный ID ячейки
LAC — Location Area Code ячейки
OperatorID — оператор (1 — МТС, 99 — Билайн...)
eventid — идентификатор измерения
radio — тип станции (1 — GSM, 2 — LTE, 3 — UMTS)

LocationTimestampDelta — время от определения положения до конца часа, с
LocationAltitude — высота, м
LocationDirection — направление телефона
LocationPrecision — точность определения положения, м
LocationSpeed — скорость движения

cell_lat — широта вышки
cell_lon — долгота вышки
range — "дальнобойность" вышки
