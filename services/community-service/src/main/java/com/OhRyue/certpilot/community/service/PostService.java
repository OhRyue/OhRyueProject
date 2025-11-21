package com.OhRyue.certpilot.community.service;

import com.OhRyue.certpilot.community.domain.Post;
import com.OhRyue.certpilot.community.domain.PostCategory;
import com.OhRyue.certpilot.community.domain.ReactionTargetType;
import com.OhRyue.certpilot.community.dto.CommentDtos;
import com.OhRyue.certpilot.community.dto.PageDto;
import com.OhRyue.certpilot.community.dto.PostDtos;
import com.OhRyue.certpilot.community.exception.ForbiddenException;
import com.OhRyue.certpilot.community.exception.ResourceNotFoundException;
import com.OhRyue.certpilot.community.repository.PostRepository;
import com.OhRyue.certpilot.community.repository.ReactionRepository;
import io.micrometer.core.annotation.Timed;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Clock;
import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class PostService {

    private final PostRepository postRepository;
    private final CommentService commentService;
    private final ReactionRepository reactionRepository;
    private final CategoryService categoryService;
    private final ViewLogService viewLogService;
    private final ModerationService moderationService;
    private final Clock clock = Clock.systemUTC();

    @Timed(value = "community.posts.list", histogram = true)
    @Transactional(readOnly = true)
    public PostDtos.PostListResponse list(PostDtos.PostQuery query,
                                          int page,
                                          int size,
                                          String viewerId) {
        Pageable pageable = PageRequest.of(Math.max(0, page), Math.max(1, Math.min(size, 100)), toSort(query.sort()));
        Byte categoryId = resolveCategoryId(query.categoryCode());
        boolean anonymousOnly = Boolean.TRUE.equals(query.anonymousOnly());
        boolean mine = Boolean.TRUE.equals(query.mine());
        String authorId = mine ? requireViewerId(viewerId, "나의 활동 필터는 로그인 후 이용 가능합니다.") : null;
        Instant since = computeSince(query.days());

        List<String> blockedAuthors = viewerId == null
                ? List.of()
                : moderationService.blockedUserIds(viewerId).stream().toList();

        Page<Post> result;
        if (blockedAuthors.isEmpty()) {
            result = postRepository.search(
                    categoryId,
                    sanitizeKeyword(query.keyword()),
                    authorId,
                    anonymousOnly,
                    since,
                    pageable);
        } else {
            result = postRepository.searchExcluding(
                    categoryId,
                    sanitizeKeyword(query.keyword()),
                    authorId,
                    anonymousOnly,
                    since,
                    blockedAuthors,
                    pageable);
        }

        Map<Byte, PostCategory> categoryMap = categoryService.findAll().stream()
                .collect(Collectors.toMap(PostCategory::getId, Function.identity()));

        List<PostDtos.PostSummary> summaries = result.getContent().stream()
                .map(post -> toSummary(post, categoryMap))
                .collect(Collectors.toList());

        return new PostDtos.PostListResponse(PageDto.of(result), summaries);
    }

    @Timed(value = "community.posts.activity", histogram = true)
    @Transactional(readOnly = true)
    public PostDtos.MyActivityResponse myActivity(String userId, int postLimit, int commentLimit) {
        String normalizedUserId = requireViewerId(userId, "사용자 식별자가 필요합니다.");
        int safePostLimit = Math.max(1, Math.min(postLimit, 50));

        Pageable postPage = PageRequest.of(0, safePostLimit, Sort.by(Sort.Order.desc("createdAt")));
        List<Post> posts = postRepository.findByAuthorIdAndDeletedAtIsNull(normalizedUserId, postPage);

        Map<Byte, PostCategory> categoryMap = categoryService.findAll().stream()
                .collect(Collectors.toMap(PostCategory::getId, Function.identity()));

        List<PostDtos.PostSummary> myPosts = posts.stream()
                .map(post -> toSummary(post, categoryMap))
                .toList();

        List<CommentDtos.CommentResponse> myComments = commentService.recentByUser(normalizedUserId, commentLimit);

        return new PostDtos.MyActivityResponse(myPosts, myComments);
    }

    @Timed(value = "community.posts.detail", histogram = true)
    @Transactional
    public PostDtos.PostDetailResponse getDetail(Long postId, String viewerId, int commentSize) {
        Post post = postRepository.findByIdAndDeletedAtIsNull(postId)
                .orElseThrow(() -> new ResourceNotFoundException("게시글을 찾을 수 없습니다."));

        Set<String> blockedUsers = viewerId == null ? Set.of() : moderationService.blockedUserIds(viewerId);
        if (viewerId != null && blockedUsers.contains(post.getAuthorId())) {
            throw new ForbiddenException("차단한 사용자의 게시글입니다.");
        }

        post.setViewCount(post.getViewCount() + 1);
        viewLogService.record(postId, viewerId);

        Map<Byte, PostCategory> categoryMap = categoryService.findAll().stream()
                .collect(Collectors.toMap(PostCategory::getId, Function.identity()));

        boolean likedByViewer = viewerId != null &&
                reactionRepository.findByTargetTypeAndTargetIdAndUserId(ReactionTargetType.POST, postId, viewerId).isPresent();

        Page<CommentDtos.CommentResponse> commentPage = commentService.list(postId, viewerId, 0, commentSize, blockedUsers);
        List<CommentDtos.CommentResponse> comments = commentPage.getContent();

        PostDtos.PostSummary summary = toSummary(post, categoryMap);
        return new PostDtos.PostDetailResponse(
                summary,
                post.getContent(),
                comments,
                likedByViewer,
                viewerId != null && viewerId.equals(post.getAuthorId()),
                post.getUpdatedAt()
        );
    }

    @Timed(value = "community.posts.create", histogram = true)
    @Transactional
    public Post create(String authorId, PostDtos.PostCreateRequest request) {
        PostCategory category = categoryService.getByCode(request.categoryCode());
        Post post = new Post();
        post.setCategoryId(category.getId());
        post.setAuthorId(authorId);
        post.setAnonymous(request.anonymous());
        post.setTitle(request.title());
        post.setContent(request.content());
        post.setCreatedAt(Instant.now(clock));
        return postRepository.save(post);
    }

    @Timed(value = "community.posts.update", histogram = true)
    @Transactional
    public Post update(Long id, String requesterId, PostDtos.PostUpdateRequest request) {
        Post post = postRepository.findByIdAndDeletedAtIsNull(id)
                .orElseThrow(() -> new ResourceNotFoundException("게시글을 찾을 수 없습니다."));
        validateAuthor(post.getAuthorId(), requesterId);
        post.setTitle(request.title());
        post.setContent(request.content());
        post.setAnonymous(request.anonymous());
        post.setUpdatedAt(Instant.now(clock));
        return post;
    }

    @Timed(value = "community.posts.delete", histogram = true)
    @Transactional
    public void delete(Long id, String requesterId) {
        Post post = postRepository.findByIdAndDeletedAtIsNull(id)
                .orElseThrow(() -> new ResourceNotFoundException("게시글을 찾을 수 없습니다."));
        validateAuthor(post.getAuthorId(), requesterId);
        post.setDeletedAt(Instant.now(clock));
    }

    @Timed(value = "community.posts.hot", histogram = true)
    @org.springframework.cache.annotation.Cacheable(
            cacheNames = "community:hot-posts",
            key = "'d'+#days+'l'+#limit",
            unless = "#viewerId != null"
    )
    @Transactional(readOnly = true)
    public PostDtos.HotPostResponse hotPosts(int days, int limit, String viewerId) {
        int normalizedDays = Math.max(1, days);
        int normalizedLimit = Math.min(Math.max(1, limit), 50);
        Instant since = Instant.now(clock).minusSeconds(normalizedDays * 86400L);
        List<String> blockedAuthors = viewerId == null
                ? List.of()
                : moderationService.blockedUserIds(viewerId).stream().toList();
        List<Post> posts = blockedAuthors.isEmpty()
                ? postRepository.findTopHotSince(since, PageRequest.of(0, normalizedLimit))
                : postRepository.findTopHotSinceExcluding(since, blockedAuthors, PageRequest.of(0, normalizedLimit));
        Map<Byte, PostCategory> categoryMap = categoryService.findAll().stream()
                .collect(Collectors.toMap(PostCategory::getId, Function.identity()));
        List<PostDtos.PostSummary> summaries = posts.stream()
                .map(p -> toSummary(p, categoryMap))
                .collect(Collectors.toList());
        return new PostDtos.HotPostResponse(since, summaries);
    }

    @Transactional(readOnly = true)
    public PostDtos.PostListResponse today(PostDtos.SortType sort, int limit, String viewerId) {
        int safeLimit = Math.min(Math.max(1, limit), 50);
        ZoneId zone = ZoneId.of("Asia/Seoul");
        Instant startOfToday = LocalDate.now(zone).atStartOfDay(zone).toInstant();
        Pageable pageable = PageRequest.of(0, safeLimit, toSort(sort));
        List<String> blockedAuthors = viewerId == null
                ? List.of()
                : moderationService.blockedUserIds(viewerId).stream().toList();

        Page<Post> page;
        if (blockedAuthors.isEmpty()) {
            page = postRepository.search(null, null, null, false, startOfToday, pageable);
        } else {
            page = postRepository.searchExcluding(null, null, null, false, startOfToday, blockedAuthors, pageable);
        }

        Map<Byte, PostCategory> categoryMap = categoryService.findAll().stream()
                .collect(Collectors.toMap(PostCategory::getId, Function.identity()));

        List<PostDtos.PostSummary> summaries = page.getContent().stream()
                .map(post -> toSummary(post, categoryMap))
                .toList();

        return new PostDtos.PostListResponse(PageDto.of(page), summaries);
    }

    @Transactional(readOnly = true)
    public PostDtos.PostMetrics metrics(Long postId) {
        Post post = postRepository.findByIdAndDeletedAtIsNull(postId)
                .orElseThrow(() -> new ResourceNotFoundException("게시글을 찾을 수 없습니다."));
        return new PostDtos.PostMetrics(
                post.getId(),
                post.getLikeCount(),
                post.getCommentCount(),
                post.getViewCount()
        );
    }

    private Sort toSort(PostDtos.SortType sortType) {
        if (sortType == PostDtos.SortType.HOT) {
            return Sort.by(Sort.Order.desc("likeCount"), Sort.Order.desc("commentCount"), Sort.Order.desc("viewCount"));
        }
        return Sort.by(Sort.Order.desc("createdAt"));
    }

    private Byte resolveCategoryId(String categoryCode) {
        if (categoryCode == null || categoryCode.isBlank() || "ALL".equalsIgnoreCase(categoryCode)) {
            return null;
        }
        PostCategory category = categoryService.getByCode(categoryCode);
        return category.getId();
    }

    private String sanitizeKeyword(String keyword) {
        if (keyword == null || keyword.isBlank()) {
            return null;
        }
        return keyword.trim();
    }

    private Instant computeSince(Integer days) {
        if (days == null || days <= 0) {
            return null;
        }
        int normalized = Math.min(days, 30); // 최대 30일
        return Instant.now(clock).minusSeconds(normalized * 86_400L);
    }

    public PostDtos.PostSummary summarize(Post post) {
        Map<Byte, PostCategory> categoryMap = categoryService.findAll().stream()
                .collect(Collectors.toMap(PostCategory::getId, Function.identity()));
        return toSummary(post, categoryMap);
    }

    private PostDtos.PostSummary toSummary(Post post, Map<Byte, PostCategory> categoryMap) {
        PostCategory category = categoryMap.getOrDefault(post.getCategoryId(), null);
        String categoryCode = category != null ? category.getCode() : null;
        String categoryName = category != null ? category.getName() : null;
        return new PostDtos.PostSummary(
                post.getId(),
                categoryCode,
                categoryName,
                post.getTitle(),
                buildExcerpt(post.getContent()),
                post.isAnonymous(),
                post.getAuthorId(),
                displayName(post.isAnonymous(), post.getAuthorId()),
                post.getLikeCount(),
                post.getCommentCount(),
                post.getViewCount(),
                post.getCreatedAt()
        );
    }

    private String buildExcerpt(String content) {
        if (content == null) {
            return "";
        }
        String trimmed = content.trim();
        if (trimmed.length() <= 140) {
            return trimmed;
        }
        return trimmed.substring(0, 137) + "...";
    }

    private String displayName(boolean anonymous, String authorId) {
        return anonymous ? "익명" : authorId;
    }

    private void validateAuthor(String authorId, String requesterId) {
        if (requesterId == null || !requesterId.equals(authorId)) {
            throw new ForbiddenException("작성자만 수정/삭제할 수 있습니다.");
        }
    }

    private String requireViewerId(String viewerId, String message) {
        if (viewerId == null || viewerId.isBlank()) {
            throw new ForbiddenException(message);
        }
        return viewerId;
    }
}
