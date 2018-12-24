let input = '';

let list = input.split(' ').map(s=>s.replace('+','')).map(s=>parseInt(s));

return list.reduce((a,b)=>a+b,0);
