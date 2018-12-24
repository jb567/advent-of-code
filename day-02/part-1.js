let input = '';

let list = input.split(' ')

let freqs = list.map(str=> {
  let frq = {};
  for (let i=0; i < str.length; i++) {
    const char = str.charAt(i);
    let count = frq[char] || 0;
    frq[char] = count + 1;
  }
  return frq
});

const twos = freqs.filter(x=>Object.values(x).includes(2)).length
const threes = freqs.filter(x=>Object.values(x).includes(3)).length
return twos*threes;
