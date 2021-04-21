$( function() {
    let $sequentForm = $('#sequent-form');
    $sequentForm.on('submit', function(e) {
        e.preventDefault(); // avoid to execute the actual submit of the form.
    });

    // Parse URL and auto-complete / auto-submit sequent form
    let searchParams = new URLSearchParams(window.location.search);
    if (searchParams.has('s')) {
        $sequentForm.find($('input[name=sequentAsString]')).val(searchParams.get('s'));
        let autoReverse = searchParams.has('auto_reverse') && searchParams.get('auto_reverse') === '1';
        submitSequent($sequentForm, autoReverse);
    }
    
    // Parse URL hash
    switch (window.location.hash) {
        case '#tutorial':
            showTutorial();
            break;

        case '#rules':
            showRules();
            break;
    }
} );

// ************
// SEQUENT FORM
// ************

function submitSequent(element, autoReverse = false) {
    cleanMainProof();

    let form = $(element).closest('form');
    let sequentAsString = form.find($('input[name=sequentAsString]')).val();

    // We update current URL by adding sequent in query parameters
    addQueryParamInUrl('s', sequentAsString.toString(), 'Linear logic proof start');

    parseSequentAsString(sequentAsString, $('#main-proof-container'), autoReverse);
}

function parseSequentAsString(sequentAsString, $container, autoReverse) {
    let apiUrl = '/parse_sequent';

    $.ajax({
        type: 'GET',
        url: apiUrl,
        data: { sequentAsString },
        success: function(data)
        {
            if (data['is_valid']) {
                initProofWithSequent(data['sequent_as_json'], $container, {
                    withInteraction: true,
                    exportButtons: true,
                    checkProvability: true,
                    autoReverseOption: true,
                    autoReverse: autoReverse,
                    onAutoReverseToggle: onAutoReverseToggle
                });
            } else {
                displayPedagogicError(data['error_message'], $container);
            }
        },
        error: onAjaxError
    });
}

function cleanMainProof() {
    $('#main-proof-container').html('');
}

// ********
// TUTORIAL
// ********

function showTutorial() {
    // Create tutorial proof
    $('.tutorial .proof-container').each(function (i, container) {
        let $container = $(container);
        let sequent = JSON.parse(uncompressJson($container.html()));
        $container.html('');
        initProofWithSequent(sequent, $container, {
            withInteraction: true
        });
    })

    $('.tutorial').removeClass('hidden');
}

// *****
// RULES
// *****

function showRules() {
    // Create rules proof
    $('.rules .proof-container').each(function (i, container) {
        let $container = $(container);
        let proofAsJson = JSON.parse(uncompressJson($container.html()));
        $container.html('');
        initProof(proofAsJson, $container);
    })

    $('.rules').removeClass('hidden');
}

// *******************
// AUTO-REVERSE OPTION
// *******************

function onAutoReverseToggle(autoReverse) {
    if (autoReverse) {
        addQueryParamInUrl('auto_reverse', '1', 'Auto reverse mode set to true');
    } else {
        addQueryParamInUrl('auto_reverse', null, 'Auto reverse mode set to false');
    }
}

// *****
// UTILS
// *****

function addQueryParamInUrl (key, value, title) {
    let currentUrl = new URL(window.location.href);

    if (value !== null) {
        currentUrl.searchParams.set(key, value);
    } else {
        currentUrl.searchParams.delete(key);
    }
    currentUrl.hash = '';
    window.history.pushState(value, title, currentUrl.toString());
}