const fs = require('fs')
let inp = fs.readFileSync("./test/input.txt").toString('utf-8').trim()

let garbage = false, score = 0.0, depth = 1, garbageCount = 0
for (let i = 0, c = inp[0]; i < inp.length; i++, c = inp[i]) {
  if (c == '!') i++
  else if (garbage && c != '>') garbageCount++
  else if (c == '<') garbage = true
  else if (c == '>') garbage = false
  else if (c == '{') score += depth++
  else if (c == '}') depth--
}
console.log(score, garbageCount);
