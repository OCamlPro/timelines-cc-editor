var logo = new Vue({
    el: '#logo',
    data: {message: 'EZ-Timeline'}
})

var home = new Vue({
    el: '#nav-home',
    data: {
	message: 'Home'
    }
})

var create = new Vue({
    el: '#nav-create',
    data: {
	message: 'Get started'
    }
})

var title = new Vue ({
    el:'#title',
    data:{
	message: 'Welcome to EZ-Timeline'
    }
})

var subtitle = new Vue ({
    el:'#subtitle',
    data:{
	message: 'The simplest way to organize your story'
    }
})

var create = new Vue ({
    el:'#create-timeline-title',
    data:{
	message: 'Create your own timeline'
    }
})

var create_parag = new Vue ({
    el:'#create-descr',
    data:{
	message: 'Click below to create your own timeline. You may enter a name and a short description (the latter can be modified)'
    }
})

var create_button = new Vue({
    el: '#create',
    data:{
	message: 'Create'
    },
    methods: {
	click: function (){
	    this.message = this.message.split('').reverse().join('')
	}
    }
})

var share = new Vue ({
    el:'#share-title',
    data:{
	message: 'Share your timeline with everyone'
    }
})

var share_descr = new Vue ({
    el:'#share-descr',
    data:{
	message: 'You can share your timeline with other without giving the rights to edit it. Select the timeline you want to export.'
    }
})

var name_input = new Vue({
    el:'#timeline-name',
    data:{
	placeholder: 'Name (will be used as URL)',
	help: 'Enter the name of your timeline'
    }
})

var descr_input = new Vue({
    el:'#timeline-description',
    data:{
	placeholder: 'Description',
	help: 'Enter the description of your timeline'
    }
})

var descr_input = new Vue({
    el:'#timeline-share-name',
    data:{
	placeholder: 'Timeline name (or url)',
	help: 'Enter the name (or the URL) of your timeline'
    }
})

var create_button = new Vue({
    el: '#share',
    data:{
	message: 'Share'
    },
    methods: {
	click: function (){
	    this.message = this.message.split('').reverse().join('')
	}
    }
})
