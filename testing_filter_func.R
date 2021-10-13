a = c(1,2,3,4,5,6, 7, 8)
innov = a
innov[1] = 0
for (i in 2:length(a)) {
  innov[i] = a[i] - 0.5 * innov[i-1] - 0.5 * a[i-1]
}
innov
a1 = stats::filter(innov, c(1, 0.5), sides = 1)
a1[1] = 0
a1
a2 = stats::filter(a1, c(0.5), method = "recursive")
a2
