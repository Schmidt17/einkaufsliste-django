class CustomModal extends HTMLElement {

    constructor() {
        super();
    }

    connectedCallback() {
        M.Modal.init(this.querySelector('.modal'));
    }


}


customElements.define("custom-modal", CustomModal);