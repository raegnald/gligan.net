
document.addEventListener("DOMContentLoaded", (_) => {
    const toggles = document.querySelectorAll('#content h2 span, h3 span, h4 span, h5 span, h6 span');

    toggles.forEach((toggle) => {
        toggle.addEventListener('click', () => {
            const sectionTitle = toggle.parentElement;;
            const sectionSubsections = sectionTitle.parentElement.querySelectorAll('div');

            toggle.classList.toggle('section-collapsed');

            sectionTitle.classList.toggle('org-section-title-hidden');

            sectionSubsections.forEach((subsection) => {
                subsection.classList.toggle('org-section-hidden');
            });

        });
    });
});
