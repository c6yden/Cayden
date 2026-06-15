
const mass         =     0; 
const xSpeed       =     1;
const ySpeed       =     2;
const xPos         =     3; 
const yPos         =     4;
let   tall         =     0; 
let   wide         =     0; 
let   particle     =    [];
let   loopStarted  = false;
let   loop;



function clearArray(){
  particle = [];
  loopStarted = false;
}
clear.addEventListener("click", clearArray);



let repulseForce = 5; 
document.getElementById("repulseSlider").oninput = function() {
repulseForce = this.value / 10;}


let bodyCount = 100; 
document.getElementById("countSlider").oninput = function() {
bodyCount = this.value;}


let pushRadius = 100; 
document.getElementById("forceDistSlider").oninput = function() {
pushRadius = this.value;}


let spawnBuffer = 40;
document.getElementById("spawnDistSlider").oninput = function() {
spawnBuffer = Number(this.value);}


let speed = 0; 
document.getElementById("spawnSpeedSlider").oninput = function() {
speed = this.value;}


let radius = 5; //in pixels
document.getElementById("sizeSlider").oninput = function() {
radius = this.value;}


let friction =  .998; //percent of speed that is kept per tick
document.getElementById("frictionSlider").oninput = function() {
friction = (this.value) / 1000;}








function startLoop(){

  if (!loopStarted) {
    loopStarted = true

    loop = setInterval(() => {

      tall =  window.innerHeight;
      wide =  window.innerWidth ;
      repulse    ();
      checkBounds();
      temp       ();
      display    ();

    }, 1);
  }
}
document.addEventListener("keydown", function (event) {if (event.key === "Enter") {startLoop();}});


function temp(){
  for (let i = 0; i < particle.length; i++){
    particle[i][xSpeed] += (random(-100,100))/100000
    particle[i][ySpeed] += (random(-100,100))/100000
  }

}


function checkBounds(){
  for (let i = 0; i < particle.length; i++){
    if (particle[i][xPos] <= 0){
      particle[i][xPos  ]  = 0;
      particle[i][xSpeed]  = -particle[i][xSpeed]/2;
    }
    if (particle[i][xPos] >= wide) {
      particle[i][xPos  ]  = wide;
      particle[i][xSpeed]  = -particle[i][xSpeed]/2;
    }
    if (particle[i][yPos] >= tall) {
      particle[i][yPos  ]  = tall;
      particle[i][ySpeed]  = -particle[i][ySpeed]/2;
    }
    if (particle[i][yPos] <= 0) {
      particle[i][yPos  ]  = 0;
      particle[i][ySpeed]  = -particle[i][ySpeed]/2;
    }
    particle[i][xSpeed] = particle[i][xSpeed]* (friction);
    particle[i][ySpeed] = particle[i][ySpeed]* (friction);
  }
}





function repulse(){

  for   (let i = 0    ; i < particle.length - 1; i++){ 
    for (let j = i + 1; j < particle.length    ; j++){  

      let dx = particle[j][xPos] - particle[i][xPos];
      let dy = particle[j][yPos] - particle[i][yPos];


      let distance = Math.sqrt(dx * dx + dy * dy);

      if (distance > pushRadius) continue;
      if (distance < 5) distance = 5;
    
      let force  = repulseForce / (distance * distance);
      let xForce = force * (dx  / distance            );
      let yForce = force * (dy  / distance            );   

      particle[i][xSpeed] -= xForce;
      particle[i][ySpeed] -= yForce; 

      particle[j][xSpeed] += xForce; 
      particle[j][ySpeed] += yForce; 
    }
  }


  for (let i = 0; i < particle.length; i++){ 

    let distance = particle[i][xPos]
    if (distance < pushRadius){
      if (distance < 5) distance = 5;
      let xForce = repulseForce / (distance * distance);
      particle[i][xSpeed] += xForce;
    }

    let distance2 = particle[i][yPos]
    if (distance2 < pushRadius){
      if (distance2 < 5) distance2 = 5;
      let yForce = repulseForce / (distance2 * distance2);
      particle[i][ySpeed] += yForce;
    }

    let distanceXRight = particle[i][xPos] - wide;
    if (Math.abs(distanceXRight) < pushRadius) {
      if (Math.abs(distanceXRight) < 5) {
        distanceXRight = distanceXRight < 0 ? 5 : -5;
      }
      let xForce = repulseForce / (distanceXRight * distanceXRight);
      particle[i][xSpeed] -= xForce;
    } 

    let distanceYBottom = particle[i][yPos] - tall;
    if (Math.abs(distanceYBottom) < pushRadius) {
      if (Math.abs(distanceYBottom) < 5) {
        distanceYBottom = distanceYBottom < 0 ? 5 : -5;
      }
      let yForce = repulseForce / (distanceYBottom * distanceYBottom);
      particle[i][ySpeed] -= yForce;
    }
  }

  for (let i = 0; i < particle.length; i++){
    particle[i][xPos] += particle[i][xSpeed];
    particle[i][yPos] += particle[i][ySpeed];
  }
}







function display(){

  let world = document.querySelector(".bodies");
  while (world.children.length > particle.length - 1) {
    world.removeChild(world.lastChild);
  }

  for (let i = 0; i < particle.length; i ++){

    let body = document.getElementById("body" + i);
    if (!body) {
      body = document.createElement("div");
      body.className = "body";
      body.id = "body" + i;
      world.appendChild(body);
    }

    let xValue = particle[i][xPos];
    let yValue = particle[i][yPos];

    Object.assign(body.style, {
      left:   xValue   + "px",
      top:    yValue   + "px",
      width:  radius*2 + "px",
      height: radius*2 + "px"
    });
  }
}

// ----------------------------------------------------- //
// particle spawning
// ----------------------------------------------------- //

document.addEventListener('mousedown', function(event) {
  let x = event.pageX; 
  let y = event.pageY;
  
  if (x < 1100 && y < 90) return;

  randomize(x,y);
  display();
  startLoop();
});

function randomize(x,y){

  clearInterval(loop);
  loopStarted = false;
  let counter =     0;

  while (counter < bodyCount){
    counter++
    particle.push([
      100,
      random( -speed, speed               )/10, 
      random( -speed, speed               )/10, 
      random( x-spawnBuffer, x+spawnBuffer), 
      random( y-spawnBuffer, y+spawnBuffer) 
    ]);
  }
}
function random(min, max){return Math.floor(Math.random() * (max - min + 1)) + min;}
