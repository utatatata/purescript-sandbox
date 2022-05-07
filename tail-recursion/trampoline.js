function fact(n) {
  if (n === 0) {
    return 0;
  } else {
    return n + fact(n - 1);
  }
}

function even(n) {
  return n === 0 ? true : odd(n - 1)
}

function odd(n) {
  return n === 0 ? false : even(n - 1)
}

console.log(even(10))
console.log(odd(10))
// console.log(odd(1000000))

function factT(acc, n) {
  if (n === 0) {
    return acc;
  } else {
    return () => factT(n + acc, n - 1);
  }
}

function evenT(n) {
  return n === 0 ? true : () => oddT(n - 1)
}

function oddT(n) {
  return n === 0 ? false : () => evenT(n - 1)
}

function runTrampoline(t) {
  while(true) {
    if (t.constructor === Function) {
      t = t()
    } else {
      return t
    }
  }
}

console.log(runTrampoline(evenT(10)))
console.log(runTrampoline(oddT(10)))
console.log(runTrampoline(oddT(1000000)))
// console.log(fact(20000))
console.log(runTrampoline(factT(0, 10)))
console.log(runTrampoline(factT(0, 20000)))
