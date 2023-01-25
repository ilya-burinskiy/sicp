(count-change 11 5)
  (count-chage 11 4)
    (count-chage 11 3)
      (count-chage 11 2)
        (count-chage 11 1)
          (count-chage 11 0)
            0
          (count-change 10 1)
            ...
            (count-change 1 1)
              1
        (count-change 6 2)
          (count-change 6 1)
            ...
            (count-change 1 1)
              1
          (count-change 1 2)
            1
      (count-change 1 3)
        1
    (count-change -14 4)
      0
  (count-change -39 5)
    0

S - размениваемая сумма
n - тип монеты
d(n) - "ценность" монеты

N(S, n) = N(S, n - 1) + N(S - d(n), n)
N(S, n - 1) = N(S, n - 2) + N(S - d(n - 1), n - 1)
...
N(S, 1) = 0 + N(S - d(1), 1)


N(S, n) = N(S - d(1), 1) + N(S - d(2), 2) + ... + N(S - d(n), n)
N(S - d(n), n) = N(S - d(n) - d(1), 1) + N(S - d(n) - d(2)) + ... + N(S - 2 * d(n), n)
...
N(S - m * d(n), n) = N(S - m * d(n) - d(1), 1) + N(S - m * d(n) - d(2)) + ... + N(S - (m + 1) * d(n), n)
S = m * d(n) + r, r < d(n)
S - (m + 1) * d(n) < 0, тогда рекурсивный процесс остановиться

N(S, n) = N(S - d(1), 1)         + N(S - d(2), 2)         + ... + N(S - d(n - 1), n - 1) +
          N(S - d(n) - d(1), 1)  + N(S - d(n) - d(2), 2)  + ... + N(S - d(n) - d(n - 1)) + ... +
          N(S - m * d(n) - d(1)) + N(S - m * d(n) - d(2)) + ... + N(S - m * d(n) - d(n - 1))

N(S - d(1), 1) потребует минимум S - d(1) шагов (это если за d(1) взять 1)
или если S = m1 * d(1) + r1, то потребуется m1 шагов (m1 = (S - r1) / d(1), т.е. m - линейно зависит от S)

N(S - d(2), 2) = N(S - d(2), 1) + N(S - 2d(2), 2)
N(S - d(2), 1) = 

N(S - d(2), 2) потребует S - d(2) шагов плюс N(S - 2 * d(2), 2) шагов
N(S - 2 * d(2), 2) потребует S - 2 * d(2) шагов плюс N(S - 3 * d(2))
S = m2 * d(2) + r2, тогда потребуется S - d(2) + S - 2d(2) + ... + S - m2*d(2) = m2*S - (d(2) * (1 + 2 + ... + m2)) = m2*S - d(2) * (m2*(m2 + 1)/2)
m2 = (S - r) / d(2)
S*(S - r) / d(2) - d(2) * ()
