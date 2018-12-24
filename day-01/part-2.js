let input = '';

let list = input.split(' ').map(s=>s.replace('+','')).map(s=>parseInt(s))

let visited=[0],
  current = 0;

while(true){
  for (let delta of list) {
    current += delta
    if (visited.indexOf(current) !== -1) {
      console.log(current);
      return
    }
    visited.push(current);
  }
}
