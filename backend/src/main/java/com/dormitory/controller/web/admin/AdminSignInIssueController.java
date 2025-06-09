package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.SignInIssueDTO;
import com.dormitory.controller.qry.SignInIssueQry;
import com.dormitory.controller.vo.SignInIssueVO;
import com.dormitory.service.SignInIssueService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 考勤发布表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/sign/in/issue")
@Api(value = "AdminSignInIssueController", tags = {"考勤发布接口"})
public class AdminSignInIssueController {

    /**
     * 考勤发布Service
     */
    private final SignInIssueService signInIssueService;


    @ApiOperation("考勤信息分页查询")
    @PostMapping("/page")
    public R<IPage<SignInIssueVO>> pageByQry(@RequestBody SignInIssueQry qry) {
        return R.success(signInIssueService.pageByQry(qry));
    }

    @ApiOperation("考勤信息详情")
    @GetMapping("/detail/{signInId}")
    public R<SignInIssueVO> detail(@PathVariable @NotNull(message = "考勤ID") Long signInId) {
        return R.success(signInIssueService.detailById(signInId));
    }

    @ApiOperation("考勤信息发布")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid SignInIssueDTO dto) {
        return R.success(signInIssueService.add(dto));
    }

    @ApiOperation("考勤信息作废")
    @PostMapping("/status/{signInId}")
    public R<Boolean> status(@NotNull(message = "考勤ID") @PathVariable Long signInId) {
        return R.success(signInIssueService.status(signInId));
    }

}

