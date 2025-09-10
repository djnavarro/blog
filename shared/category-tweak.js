
function limitCategoryList() {
  const listing = document.getElementsByClassName("quarto-listing-category").item(0)
  const categories = listing.children
  for (let ii = 0; ii < categories.length; ii++) {
    const category = categories[ii];
    const count = category.children[0].innerHTML
    if (count === "(1)") category.style.display = "none"
    if (count === "(2)") category.style.display = "none"
  }
  listing.style.display = "block"
}

limitCategoryList()
