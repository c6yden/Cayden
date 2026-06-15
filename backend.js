function updateSize() {
  const wide = window.innerWidth;

  const title    = document.getElementById("title")   ;
  const subTitle = document.getElementById("subTitle");
  const home     = document.getElementById("home")    ;
  const csv      = document.getElementById("csv")     ;
  const code     = document.getElementById("code")    ;
  const code2    = document.getElementById("code2")   ;

  const bird     = document.getElementById("bird")    ;
  const shasta   = document.getElementById("shasta")  ;

  const one      = document.getElementById("one")     ;
  const two      = document.getElementById("two")     ;

  const img1      = document.getElementById("img1")   ;
  const img2      = document.getElementById("img2")   ;

  let center = wide / 2;


  if (title)    title.   style.left = center + "px";
  if (subTitle) subTitle.style.left = center + "px";
  if (home)     home.    style.left = center + "px";
  if (csv)      csv.     style.left = center - 100+ "px";
  if (code2)    code2.   style.left = center + 100+ "px";
  if (code)     code.    style.left = center + "px";

  if (bird)   bird.  style.left = wide / 2-200 + "px";
  if (shasta) shasta.style.left = wide / 2+200 + "px";

  if (img1)   img1.  style.left = wide / 2-200 + "px";
  if (img2) img2.style.left = wide / 2+200 + "px";





  if (one)    one.   style.left = wide / 2 + "px";
  if (two) two.style.left = wide / 2 + "px";


  if (one) document.getElementById("one").textContent = `
    My name is Cayden and I'm a ---- year ---- student. I have a passion for photography and mountaineering.

    This website was made to present the work I've done in GEOG 490, at the University of ----, as well
    as reflect my general knowledge in web develoment. The images in the 'graphics' section are produced using R 
    and many R packages. These graphics detail many aspects of the demographics of San Diego County, but can be
    easily adapted to any other county in the country. They show a variety of data, but are mostly focused on sex
    income, and education. Within 'R code' are two varibles labled "county" and "state". By updating these and
    re-running this program, you can populate the graphs with the desired location. The 'R code' section containes 
    images of the code as well as links to my github repositry where it can be downloaded. The final; section 
    'JS Code' containes some of my personal projects. Continue scrolling to see more about me.
    `;


   if (two) document.getElementById("two").textContent = `
    CV // work experience // skills here // Cayden is highly qualified
    ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
    ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- 
    ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- ----
    ---- ---- ---- ---- ---- ---- ---- ---- ---- ---- --Insert headshot here //
    this imfornation is left out for privacy
    `;




  const density  = document.getElementById("density");
  const water    = document.getElementById("water")  ;
  const pyramid  = document.getElementById("pyramid");
  const collect  = document.getElementById("collect");
  const race     = document.getElementById("race")   ;



  if (density) density.style.left = center - 200 + "px";
  if (water)     water.style.left = center - 200 + "px";
  if (pyramid) pyramid.style.left = center - 200 + "px";
  if (collect) collect.style.left = center - 200 + "px";
  if (race)       race.style.left = center - 200 + "px";

  // density pyramid water collect race



  const link     = document.getElementById("link")   ; 
  if (link)       link.style.left = center - 200 + "px"; 


  const des1 = document.getElementById("des1");
  const des2 = document.getElementById("des2");
  const des3 = document.getElementById("des3");
  const des4 = document.getElementById("des4");
  const des5 = document.getElementById("des5");

  if (des1) des1.style.left = center + 250 + "px";
  if (des2) des2.style.left = center + 250 + "px";
  if (des3) des3.style.left = center + 250 + "px";
  if (des4) des4.style.left = center + 250 + "px";
  if (des5) des5.style.left = center + 250 + "px";


  if (des1) document.getElementById("des1").textContent = `
  Like much of the code below, this section uses gglpot to make 
  a color-coded map, by tract, of population density within a county
  `;

  if (des2) document.getElementById("des2").textContent = `
  This section produces a standard popualtiuon pyramid using age and sex.
  `;

  if (des3) document.getElementById("des3").textContent = `
  This is more specific code used for locations with lots of water bodies.
  Otter tail county has the most lakes in the us, so its a good idea to clearly mark them
  as the census data will have no indication of them.
  `;

  if (des4) document.getElementById("des4").textContent = `
  This section shows how the data is collected here. There is two methods, get_acs, which
  samples part of the popualtion and provides a margin of error. And get_estimates, which does not.
  `;

  if (des5) document.getElementById("des5").textContent = `
  This final section is a bar chart for the count of people who identiy with one primary race. 
  `;

  const box1  =  document.getElementById("box1");
  const box2  =  document.getElementById("box2");
  const box3  =  document.getElementById("box3");
  const box4  =  document.getElementById("box4");
  const box5  =  document.getElementById("box5");
  const box6  =  document.getElementById("box6");
  const box7  =  document.getElementById("box7");
  const box8  =  document.getElementById("box8");
  const box9  =  document.getElementById("box9");
  const box10  =  document.getElementById("box10");


  if (box1)   box1.style.left = center + 250 + "px";
  if (box2)   box2.style.left = center + 250 + "px";
  if (box3)   box3.style.left = center + 250 + "px";
  if (box4)   box4.style.left = center + 250 + "px";
  if (box5)   box5.style.left = center + 250 + "px";
  if (box6)   box6.style.left = center + 250 + "px";
  if (box7)   box7.style.left = center + 250 + "px";
  if (box8)   box8.style.left = center + 250 + "px";
  if (box9)   box9.style.left = center + 250 + "px";
  if (box10)  box10.style.left = center + 250 + "px";




  const figure1  =   document.getElementById("figure1");
  const figure2  =   document.getElementById("figure2");
  const figure3  =   document.getElementById("figure3");
  const figure4  =   document.getElementById("figure4");
  const figure5  =   document.getElementById("figure5");
  const figure6  =   document.getElementById("figure6");
  const figure7  =   document.getElementById("figure7");
  const figure8  =   document.getElementById("figure8");
  const figure9  =   document.getElementById("figure9");


 
  if (figure1)   figure1.style.left = center - 200 + "px";
  if (figure2)   figure2.style.left = center - 200 + "px";
  if (figure3)   figure3.style.left = center - 200 + "px";
  if (figure4)   figure4.style.left = center - 200 + "px";
  if (figure5)   figure5.style.left = center - 200 + "px";
  if (figure6)   figure6.style.left = center - 200 + "px";
  if (figure7)   figure7.style.left = center - 200 + "px";
  if (figure8)   figure8.style.left = center - 200 + "px";
  if (figure9)   figure9.style.left = center - 200 + "px";













  if (box1)    box1.textContent = "The amount of people within the county borders for each of the 6 largest race groups.";
  if (box2)    box2.textContent = "A popualtion pyramid split by sex, for all 5-year age groups.";
  if (box3)    box3.textContent = "Population density heat map, by tract.";
  if (box4)    box4.textContent = "Tracts are catagorized by levels of urbanization.";
  if (box5)    box5.textContent = "Similar to the graph above, this image counts the tracts in each group.";
  if (box6)    box6.textContent = "This map shows hotspots of high and low age. The more rural areas to the northeast have more older residents while the most urban areas are dominated by younger people.";
  if (box7)    box7.textContent = "Count of white men per tract. highlights urban centers and universities.";
  if (box8)    box8.textContent = "Simple income map with water bodies filled in";
  if (box9)    box9.textContent = "More complex, and discrete, income map.";


 







}

window.addEventListener("resize", updateSize);
window.addEventListener("load", updateSize);
updateSize();




