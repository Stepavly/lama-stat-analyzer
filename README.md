# Статический анализатор параметризованных байт кодов языка Lama

Исходный код анализатора находится в byterun/byterun.cpp

## Сборка

Для сборки необходима библиотека `g++-multilib` (можно установить через `sudo apt-get install g++-multilib`). Собрать анализатор можно через команду `make all`. Собранный артефакт будет находится в `byterun/byterun`.

## Пробный запуск

Пример запуска для `Sort.lama` можно посмотреть в `run.sh`. Для запуска скрипта необходимо выполнить `chmod u+x run.sh && ./run.sh`.