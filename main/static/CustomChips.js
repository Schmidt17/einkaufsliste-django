class CustomChips extends HTMLElement {
    static observedAttributes = ["changes"];

    constructor() {
        super();
    }

    connectedCallback() {
        this.tags = [];
        this.updateTags();

        this.initChips();

        this.setAttribute("changes", 0);
    }

    attributeChangedCallback(name, oldValue, newValue) {
        if (name == "changes") {
            this.updateTagsFromMaterializeChips();
            this.dispatchEvent(new CustomEvent('tagsChanged', {detail: {tags : this.tags}}))
        }
    }

    updateTags() {
        const tagElements = this.querySelectorAll('chips-tag');
        const tagList = [...tagElements];
        this.tags = tagList.map((elt) => elt.value);
    }

    updateTagsFromMaterializeChips() {
        let chipsInstance = M.Chips.getInstance(this.chipsDiv);
        let tagsData = chipsInstance.chipsData;
        this.tags = tagsData.map((elt) => elt.tag);
    }

    initChips() {

        const tags = this.tags.map((value) => Object({tag : value}));

        this.chipsDiv = document.createElement('div');
        this.chipsDiv.classList = this.classList;
        this.chipsDiv.placeholder = this.placeholder;

        this.appendChild(this.chipsDiv);

        let autocompsRaw = this.getAttribute('autocompleteOptions');
        let autocompsEntries = JSON.parse(autocompsRaw);
        let autocomps = {};
        for (var entry of autocompsEntries) {
            autocomps[entry] = null;
        }

        const thisElement = this;
        M.Chips.init(this.chipsDiv, {
            placeholder: 'Tags',
            autocompleteOptions: {
              data: autocomps,
              limit: Infinity,
              minLength: 1
            },
            data: tags,
            onChipAdd: function(e, chip) {
                thisElement.dispatchEvent(new CustomEvent('inputChanged', {detail: {remainingText : ""}}));
                thisElement.incrementChanges();
            },
            onChipDelete: function(e, chip) {
                thisElement.incrementChanges();
            }
        });

        this.chipsInput = this.chipsDiv.querySelector('input');
        this.chipsInput.addEventListener("input", function() {
            thisElement.dispatchEvent(new CustomEvent('inputChanged', {detail: {remainingText : thisElement.chipsInput.value}}));
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