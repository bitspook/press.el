var hireMeButton = document.getElementById('hire-me-btn');
var hireMeDialog = document.getElementById('hire-me-dialog');
var toggleClass = function (el, className) {
    if (el.classList.contains(className)) {
        el.classList.remove(className);
    }
    else {
        el.classList.add(className);
    }
    return el;
};
hireMeButton.addEventListener('click', function (e) {
    toggleClass(hireMeButton, 'hire-status--expanded');
});
//# sourceMappingURL=main.js.map