# super-cow

[![Build Status](https://travis-ci.org/cmc-haskell-2017/project-template.svg?branch=master)](https://travis-ci.org/cmc-haskell-2017/project-template)

Шаблон проекта для выполнения практического задания.

## Сборка и запуск

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать и запустить проект можно при помощи команды

```
stack build && stack exec super-cow
```

Запустить тесты можно при помощи команды

```
stack test
```

Чтобы запустить интепретатор GHCi и автоматически подгрузить все модули проекта, используйте команду

```
stack ghci
```

## Управление 

Управление осуществляется стрелками. Пауза и перезапуск игры осуществляется
по нажатию пробела.

![Игра «Super Cow»](images/game-preview.gif)

## Докучентация

Открыть ее можно, используя команду

```
open doc/index.html
```
