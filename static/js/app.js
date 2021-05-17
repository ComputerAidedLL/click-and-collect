$( function() {
    let $sequentForm = $('#sequent-form');
    $sequentForm.on('submit', function(e) {
        e.preventDefault(); // avoid to execute the actual submit of the form.
    });

    $('.dialog').each(function (i, item) {
        $(item).dialog({autoOpen: false, width: 500});
    })

    // Parse URL and auto-complete / auto-submit sequent form
    let sequentParam = getQueryParamInUrl('s');
    if (sequentParam !== null) {
        $sequentForm.find($('input[name=sequentAsString]')).val(sequentParam);
        submitSequent($sequentForm, true);
    }

    // Parse URL and auto-complete / auto-submit sequent form
    let compressedProofParam = getQueryParamInUrl('p');
    if (compressedProofParam !== null) {
        uncompressProof(compressedProofParam, $('#main-proof-container'));
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

function submitSequent(element, autoSubmit = false) {
    let form = $(element).closest('form');
    let sequentAsString = form.find($('input[name=sequentAsString]')).val();

    if (!autoSubmit) {
        clearSavedProof();

        // We update current URL by adding sequent in query parameters
        addQueryParamInUrl('s', sequentAsString.toString(), 'Linear logic proof start');
    }

    // Add GA events
    gtag('event', 'submit-sequent', {
        'event_category': 'user-action',
        'event_label': autoSubmit ? 'auto-submit' : 'manual-submit',
        'value': sequentAsString
    });

    parseSequentAsString(sequentAsString, $('#main-proof-container'));
}

function parseSequentAsString(sequentAsString, $container) {
    let apiUrl = '/parse_sequent';

    $.ajax({
        type: 'GET',
        url: apiUrl,
        data: { sequentAsString },
        success: function(data)
        {
            if (data['is_valid']) {
                initMainProof(data['proof']);
            } else {
                cleanMainProof();
                displayPedagogicError(data['error_message'], $container);
            }
        },
        error: onAjaxError
    });
}

function initMainProof(proofAsJson) {
    cleanMainProof();

    // We get autoReverse option in URL
    let autoReverse = getQueryParamInUrl('auto_reverse') === '1';

    initProof(proofAsJson, $('#main-proof-container'), {
        withInteraction: true,
        exportButtons: true,
        checkProvability: true,
        autoReverse: {
            value: autoReverse,
            onToggle: onAutoReverseToggle,
            dialog: 'auto-reverse-dialog'
        }
    });
}

function cleanMainProof() {
    $('#main-proof-container').html('');
}

// ********
// TUTORIAL
// ********

function showTutorial() {
    cleanUrlParams('Show tutorial');

    let $tutorial = $('.tutorial');
    if ($tutorial.data('init') !== true) {
        // Create tutorial proof
        $('.tutorial .proof-container').each(function (i, container) {
            let $container = $(container);
            let proof = JSON.parse(uncompressJson($container.html()));
            $container.html('');
            initProof(proof, $container, {
                withInteraction: true
            });
        })

        $tutorial.data('init', true);
    }

    $tutorial.removeClass('hidden');
}

function hideTutorial() {
    $('.tutorial').addClass('hidden');
    cleanUrlHash('Hide tutorial');
}

// *****
// RULES
// *****

function showRules() {
    cleanUrlParams('Show rules');

    let $rules = $('.rules');
    if ($rules.data('init') !== true) {
        // Create rules proof
        $('.rules .proof-container').each(function (i, container) {
            let $container = $(container);
            let proofAsJson = JSON.parse(uncompressJson($container.html()));
            $container.html('');
            initProof(proofAsJson, $container);
        })

        $rules.data('init', true);
    }
    $rules.removeClass('hidden');
}

function hideRules() {
    $('.rules').addClass('hidden');
    cleanUrlHash('Hide rules');
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

// ****************
// UNCOMPRESS PROOF
// ****************

function uncompressProof(compressedProof, $container) {
    let apiUrl = '/uncompress_proof';

    $.ajax({
        type: 'GET',
        url: apiUrl,
        data: { compressedProof },
        success: function(data)
        {
            if (data['is_valid']) {
                initMainProof(data['proof']);
            } else {
                cleanMainProof();
                displayPedagogicError(data['error_message'], $container);
            }
        },
        error: onAjaxError
    });
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

function getQueryParamInUrl (key) {
    let searchParams = new URLSearchParams(window.location.search);
    if (searchParams.has(key)) {
        return searchParams.get(key);
    }

    return null;
}

function cleanUrlHash (title) {
    let currentUrl = new URL(window.location.href);

    currentUrl.hash = '';
    window.history.pushState(null, title, currentUrl.toString());
}

function cleanUrlParams (title) {
    let currentUrl = new URL(window.location.href);

    currentUrl.searchParams.forEach(function (value, key) {
        currentUrl.searchParams.delete(key);
    })

    window.history.pushState(null, title, currentUrl.toString());
}

function copyUrlToClipboard () {
    // https://stackoverflow.com/questions/49618618/copy-current-url-to-clipboard/49618964#49618964

    let dummy = document.createElement('input'),
        text = window.location.href;

    document.body.appendChild(dummy);
    dummy.value = text;
    dummy.select();
    document.execCommand('copy');
    document.body.removeChild(dummy);
}