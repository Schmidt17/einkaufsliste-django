class CustomChips extends HTMLElement {
    static observedAttributes = ["changes"];

    constructor() {
        super();
    }

    connectedCallback() {
        this.setAttribute("changes", 0);

        this.initChips();
    }

    attributeChangedCallback(name, oldValue, newValue) {
        console.log(`${name} - old: ${oldValue}, new: ${newValue}`);
    }

    initChips() {
        const tagElements = this.querySelectorAll('chips-tag');
        const tagList = [...tagElements];
        const tags = tagList.map((elt) => Object({tag : elt.value}));

        const chipsDiv = document.createElement('div');
        chipsDiv.classList = this.classList;
        chipsDiv.placeholder = this.placeholder;

        this.appendChild(chipsDiv);

        const thisElement = this;
        M.Chips.init(chipsDiv, {
            placeholder: 'Tags',
            data: tags,
            onChipAdd: function(e, chip) {
                thisElement.incrementChanges();
            },
            onChipDelete: function(e, chip) {
                thisElement.incrementChanges();
            }
        });
    }

    incrementChanges() {
        const currentChanges = parseInt(this.getAttribute('changes'));
        this.setAttribute("changes", currentChanges + 1);
    }

}

class ChipsTag extends HTMLElement {

    constructor() {
        super();
    }

}

customElements.define("chips-tag", ChipsTag);
customElements.define("custom-chips", CustomChips);