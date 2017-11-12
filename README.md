# ScalaTestProj

Это мой первый проект на Scala. Буду рад любой конструктивной критике.

Задание: http://www.cone.ee/test_rus.html

Программа запускается через объект HelloWorld, находящийся в файле src/main/java/MainScript.scala. В этом же файле есть закомментированные куски кода. Они закомментированы, чтобы вывод был корректен. Вы можете раскомментрировать их, это поможет Вам лучше проанализировать решение.

Краткое описание алгоритма:
1) Из всех квадратов рассмотрим четверки квадратов (с учетом порядка). Отберём только те четвёрки, в которых углы, сходящиеся к центру, в сумме дают 10, а углы, сходящиеся друг с другом на сторонах, дают в сумме меньше 10.
2) Из всех полученных четверок рассмотрим четверки четверок =) (крест) (с учетом порядка). Отберём только те кресты, у готорых тройки углов (из условия) в сумме меньше 10.

В процессе решения был применён метод поиска с возвратом.